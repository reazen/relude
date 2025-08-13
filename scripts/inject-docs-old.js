#!/usr/bin/env node

/**
 * Documentation injection script for ReasonML extension modules
 * 
 * This script solves the documentation inheritance issue where functions
 * from extension modules lose their documentation when included via
 * ReasonML's `include` statement.
 * 
 * Features:
 * - Auto-discovery of documented functions (no hardcoded lists)
 * - Dynamic detection of extension modules and their targets
 * - Robust pattern matching for various function signatures
 * - Safe backup/restore mechanism
 */

const fs = require('fs');
const path = require('path');

// Configuration
const SRC_DIR = path.join(__dirname, '..', 'src');
const EXTENSIONS_DIR = path.join(SRC_DIR, 'extensions');
const BACKUP_DIR = path.join(__dirname, '..', '_docs_backup');

/**
 * Auto-discovers documented functions in an extension module
 * @param {string} extensionFilePath - Path to the extension module file
 * @returns {Map<string, object>} Map of function name to documentation info
 */
function autoDiscoverFunctions(extensionFilePath) {
  const content = fs.readFileSync(extensionFilePath, 'utf8');
  const functionDocs = new Map();

  // More robust pattern to match function documentation + definition
  // Captures multi-line signatures and various patterns
  const docPattern = /\/\*\*([\s\S]*?)\*\/\s*let\s+(\w+)\s*:[\s\S]*?(?:=|;)/g;
  
  let match;
  while ((match = docPattern.exec(content)) !== null) {
    const functionName = match[2];
    const docComment = match[1];
    
    // Clean up the documentation comment
    const cleanDoc = docComment
      .replace(/^\s*\*\s?/gm, '') // Remove leading * from each line
      .trim();
    
    functionDocs.set(functionName, {
      name: functionName,
      documentation: cleanDoc,
      raw: `/**${match[1]}*/`
    });
  }

  return functionDocs;
}

/**
 * Auto-discovers which modules include a specific extension
 * @param {string} extensionModuleName - Name of the extension module (e.g., 'Foldable')
 * @returns {Array<string>} List of file paths that include this extension
 */
function autoDiscoverTargets(extensionModuleName) {
  const targets = [];
  
  // Common directories where modules might include extensions
  const searchDirs = [
    path.join(SRC_DIR, 'list'),
    path.join(SRC_DIR, 'array'),
    path.join(SRC_DIR, 'option'),
    SRC_DIR // Root src directory
  ];
  
  // Pattern to match include statements for this extension
  const includePattern = new RegExp(
    `include\\s+Relude_Extensions_${extensionModuleName}\\.\\w+Extensions\\s*\\(`,
    'g'
  );
  
  function searchDirectory(dir) {
    if (!fs.existsSync(dir)) return;
    
    const items = fs.readdirSync(dir);
    for (const item of items) {
      const fullPath = path.join(dir, item);
      const stat = fs.statSync(fullPath);
      
      if (stat.isFile() && item.endsWith('.re')) {
        const content = fs.readFileSync(fullPath, 'utf8');
        if (includePattern.test(content)) {
          const relativePath = path.relative(path.join(__dirname, '..'), fullPath);
          targets.push(relativePath);
        }
      }
    }
  }
  
  searchDirs.forEach(searchDirectory);
  return [...new Set(targets)]; // Remove duplicates
}

/**
 * Builds extension configuration dynamically
 * @returns {Object} Configuration object with auto-discovered extensions and targets
 */
function buildExtensionConfig() {
  const config = {};
  
  // Find all extension modules in the extensions directory
  const extensionFiles = fs.readdirSync(EXTENSIONS_DIR)
    .filter(file => file.startsWith('Relude_Extensions_') && file.endsWith('.re'));
  
  for (const extensionFile of extensionFiles) {
    const extensionPath = path.join(EXTENSIONS_DIR, extensionFile);
    
    // Extract the extension type name (e.g., 'Foldable' from 'Relude_Extensions_Foldable.re')
    const extensionType = extensionFile
      .replace('Relude_Extensions_', '')
      .replace('.re', '');
    
    // Auto-discover functions
    const functionDocs = autoDiscoverFunctions(extensionPath);
    const functionNames = Array.from(functionDocs.keys());
    
    // Skip if no documented functions found
    if (functionNames.length === 0) {
      console.log(`  Skipping ${extensionFile} - no documented functions found`);
      continue;
    }
    
    // Auto-discover target modules
    const targets = autoDiscoverTargets(extensionType);
    
    // Skip if no targets found
    if (targets.length === 0) {
      console.log(`  Skipping ${extensionFile} - no target modules found`);
      continue;
    }
    
    // Build include pattern for this extension
    const includePattern = new RegExp(
      `^\\s*include\\s+Relude_Extensions_${extensionType}\\.\\w+Extensions\\s*\\(\\s*\\w+\\s*\\)\\s*;`,
      'gm'
    );
    
    config[extensionFile] = {
      functions: functionNames,
      functionDocs: functionDocs,
      targets: targets,
      includePattern: includePattern,
      extensionType: extensionType
    };
    
    console.log(`  ✓ Discovered ${extensionFile}:`);
    console.log(`    - ${functionNames.length} documented functions`);
    console.log(`    - ${targets.length} target modules`);
  }
  
  return config;
}

/**
 * Generates documentation injection for target module
 */
function injectDocumentation(targetFilePath, extensionConfig) {
  const content = fs.readFileSync(targetFilePath, 'utf8');
  
  // Find the include statement for this extension
  const includeMatch = content.match(extensionConfig.includePattern);
  
  if (!includeMatch) {
    console.log(`    Warning: No include statement found in ${targetFilePath}`);
    return content;
  }

  const includeStatement = includeMatch[0];
  
  // Generate comprehensive documentation block for all included functions
  let functionDocsText = '';
  
  for (const [functionName, docInfo] of extensionConfig.functionDocs.entries()) {
    // Format each function's documentation properly
    const lines = docInfo.documentation.split('\n');
    const formattedDoc = lines[0]; // Just use the first line for brevity
    functionDocsText += `\n * - ${functionName}: ${formattedDoc}`;
  }

  // Create a comprehensive documentation block with proper formatting
  const documentationBlock = `/**
 * The following functions are provided by including ${extensionConfig.extensionType} extensions:${functionDocsText}
 * 
 * @see Complete documentation in Relude_Extensions_${extensionConfig.extensionType}
 */`;

  // Insert documentation before the include statement
  const injectedContent = content.replace(
    includeStatement,
    `${documentationBlock}
${includeStatement}`
  );

  return injectedContent;
}

/**
 * Creates backup of original source files
 */
function createBackups(filesToModify) {
  // Clean up existing backup directory
  if (fs.existsSync(BACKUP_DIR)) {
    fs.rmSync(BACKUP_DIR, { recursive: true, force: true });
  }

  fs.mkdirSync(BACKUP_DIR, { recursive: true });
  
  console.log('\nCreating backups of original source files...');
  
  for (const filePath of filesToModify) {
    const relativePath = path.relative(path.join(__dirname, '..'), filePath);
    const backupPath = path.join(BACKUP_DIR, relativePath);
    
    // Create backup directory structure
    const backupDir = path.dirname(backupPath);
    fs.mkdirSync(backupDir, { recursive: true });
    
    // Copy original file to backup
    fs.copyFileSync(filePath, backupPath);
    console.log(`  ✓ Backed up: ${relativePath}`);
  }
}

/**
 * Restores original source files from backup
 */
function restoreBackups() {
  if (!fs.existsSync(BACKUP_DIR)) {
    console.log('No backups to restore');
    return;
  }
  
  console.log('\nRestoring original source files...');
  
  function restoreRecursive(backupDir, baseDir) {
    const items = fs.readdirSync(backupDir);
    
    for (const item of items) {
      const backupPath = path.join(backupDir, item);
      const originalPath = path.join(baseDir, item);
      const stat = fs.statSync(backupPath);
      
      if (stat.isDirectory()) {
        restoreRecursive(backupPath, originalPath);
      } else {
        fs.copyFileSync(backupPath, originalPath);
        const relativePath = path.relative(path.join(__dirname, '..'), originalPath);
        console.log(`  ✓ Restored: ${relativePath}`);
      }
    }
  }
  
  restoreRecursive(BACKUP_DIR, path.join(__dirname, '..'));
}

/**
 * Main execution function
 */
function main() {
  console.log('Documentation Injection Script');
  console.log('==============================\n');

  try {
    console.log('Auto-discovering extension modules and targets...\n');
    
    // Build configuration dynamically
    const extensionConfig = buildExtensionConfig();
    
    if (Object.keys(extensionConfig).length === 0) {
      console.log('No extension modules with documented functions found.');
      return;
    }
    
    console.log('\nInjecting documentation into target modules...\n');
    
    // Collect all files to modify
    const filesToModify = new Set();
    for (const config of Object.values(extensionConfig)) {
      for (const target of config.targets) {
        filesToModify.add(path.join(__dirname, '..', target));
      }
    }
    
    // Create backups before modifying
    createBackups(Array.from(filesToModify));
    
    // Inject documentation into each file
    console.log('\nProcessing injections...');
    for (const [extensionFile, config] of Object.entries(extensionConfig)) {
      console.log(`\n  Extension: ${extensionFile}`);
      
      for (const targetFile of config.targets) {
        const targetPath = path.join(__dirname, '..', targetFile);
        
        if (fs.existsSync(targetPath)) {
          // Inject documentation and write back
          const injectedContent = injectDocumentation(targetPath, config);
          fs.writeFileSync(targetPath, injectedContent);
          
          console.log(`    ✓ Injected into ${targetFile}`);
        }
      }
    }
    
    console.log('\n✅ Documentation injection completed successfully!');
    console.log('Source files have been temporarily modified with injected documentation.');
    console.log('Build documentation now, then run this script with --cleanup to restore originals.');
    
  } catch (error) {
    console.error('\n❌ Error during documentation injection:', error.message);
    console.error(error.stack);
    
    // Try to restore backups on error
    try {
      restoreBackups();
    } catch (restoreError) {
      console.error('❌ Failed to restore backups:', restoreError.message);
    }
    
    process.exit(1);
  }
}

/**
 * Clean up injected files and restore originals
 */
function cleanup() {
  console.log('Documentation Injection Cleanup');
  console.log('==============================\n');
  
  try {
    restoreBackups();
    
    // Clean up backup directory
    if (fs.existsSync(BACKUP_DIR)) {
      fs.rmSync(BACKUP_DIR, { recursive: true, force: true });
      console.log('\n✓ Cleaned up backup files');
    }
    
    console.log('\n✅ Cleanup completed successfully!');
    
  } catch (error) {
    console.error('❌ Error during cleanup:', error.message);
    process.exit(1);
  }
}

// Handle command line arguments
if (process.argv.includes('--cleanup')) {
  cleanup();
  process.exit(0);
}

// Run main function if script is executed directly
if (require.main === module) {
  main();
}

module.exports = {
  autoDiscoverFunctions,
  autoDiscoverTargets,
  buildExtensionConfig,
  injectDocumentation,
  createBackups,
  restoreBackups,
  cleanup
};