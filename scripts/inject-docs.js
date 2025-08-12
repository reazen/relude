#!/usr/bin/env node

/**
 * Documentation injection script for ReasonML extension modules
 * 
 * This script solves the documentation inheritance issue where functions
 * from extension modules lose their documentation when included via
 * ReasonML's `include` statement.
 * 
 * The script:
 * 1. Parses extension modules to extract function documentation
 * 2. Identifies target modules that include extension functionality
 * 3. Injects documentation comments before the include statements
 * 4. Creates temporary files for documentation generation
 */

const fs = require('fs');
const path = require('path');

// Configuration
const SRC_DIR = path.join(__dirname, '..', 'src');
const EXTENSIONS_DIR = path.join(SRC_DIR, 'extensions');
const BACKUP_DIR = path.join(__dirname, '..', '_docs_backup');

// Extension modules and their target modules
const EXTENSION_CONFIG = {
  'Relude_Extensions_Foldable.re': {
    functions: [
      'any', 'all', 'containsBy', 'contains', 'indexOfBy', 'indexOf',
      'minBy', 'min', 'maxBy', 'max', 'countBy', 'length', 'size', 'count',
      'forEach', 'forEachWithIndex', 'find', 'findWithIndex', 'toList', 'foldMap'
    ],
    targets: [
      'src/list/Relude_List_Instances.re',
      'src/array/Relude_Array_Instances.re',
      'src/option/Relude_Option_Instances.re',
      'src/Relude_Result.re',
      'src/Relude_Tree.re',
      'src/Relude_NonEmpty.re',
      'src/Relude_SequenceZipper.re'
    ],
    includePattern: /^\s*include\s+Relude_Extensions_Foldable\.FoldableExtensions\s*\(\s*\w+\s*\)\s*;/gm
  }
};

/**
 * Extracts function documentation from extension module source
 */
function extractFunctionDocs(extensionFilePath) {
  const content = fs.readFileSync(extensionFilePath, 'utf8');
  const functionDocs = new Map();

  // Pattern to match function documentation + definition
  const docPattern = /\/\*\*[\s\S]*?\*\/\s*let\s+(\w+)\s*:/g;
  
  let match;
  while ((match = docPattern.exec(content)) !== null) {
    const functionName = match[1];
    const fullMatch = match[0];
    
    // Extract just the documentation comment
    const docMatch = fullMatch.match(/\/\*\*[\s\S]*?\*\//);
    if (docMatch) {
      functionDocs.set(functionName, docMatch[0]);
    }
  }

  return functionDocs;
}

/**
 * Generates documentation injection comment for a function
 */
function generateDocInjection(functionName, originalDoc) {
  // Clean up the original documentation and add attribution
  const cleanDoc = originalDoc
    .replace(/\/\*\*/, '/**')
    .replace(/\*\//, `
  
  @see <Relude_Extensions_Foldable.html#${functionName}> Original documentation
  */`);

  return `
${cleanDoc}
let ${functionName}: 'a. ('a => bool, F.t('a)) => bool;
`;
}

/**
 * Injects documentation into target module
 */
function injectDocumentation(targetFilePath, extensionConfig, functionDocs) {
  const content = fs.readFileSync(targetFilePath, 'utf8');
  
  // Find the include statement for this extension
  const includeMatch = content.match(extensionConfig.includePattern);
  
  if (!includeMatch) {
    console.log(`  Warning: No include statement found in ${targetFilePath}`);
    return content;
  }

  const includeStatement = includeMatch[0];
  
  // Generate comprehensive documentation block for all included functions
  let functionDocsText = '';
  
  for (const functionName of extensionConfig.functions) {
    const doc = functionDocs.get(functionName);
    if (doc) {
      // Clean up the documentation
      const cleanDoc = doc
        .replace(/\/\*\*\s*\n?/, '') // Remove opening /**
        .replace(/\s*\*\/\s*$/, '') // Remove closing */
        .replace(/^\s*\*\s?/gm, '') // Remove leading * from each line
        .trim();

      functionDocsText += `
 * 
 * {b ${functionName}}: ${cleanDoc}`;
    }
  }

  // Create a comprehensive documentation block
  const documentationBlock = `
/**
 * The following functions are provided by including Foldable extensions:${functionDocsText}
 * 
 * @see <Relude_Extensions_Foldable.html> Complete documentation for all extension functions
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
  
  console.log('Creating backups of original source files...\n');
  
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
  
  console.log('Restoring original source files...\n');
  
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
 * Injects documentation into source files in place
 */
function injectDocumentationInPlace() {
  console.log('Injecting documentation into target modules...\n');

  const filesToModify = [];

  for (const [extensionFile, config] of Object.entries(EXTENSION_CONFIG)) {
    const extensionPath = path.join(EXTENSIONS_DIR, extensionFile);
    
    console.log(`Processing extension: ${extensionFile}`);
    
    // Extract function documentation from extension
    const functionDocs = extractFunctionDocs(extensionPath);
    console.log(`  Found documentation for ${functionDocs.size} functions`);

    // Process each target module
    for (const targetFile of config.targets) {
      const targetPath = path.join(__dirname, '..', targetFile);
      
      console.log(`  Processing target: ${targetFile}`);
      
      if (!fs.existsSync(targetPath)) {
        console.log(`    Warning: Target file not found: ${targetPath}`);
        continue;
      }

      filesToModify.push(targetPath);
    }
    
    console.log('');
  }

  // Create backups before modifying
  createBackups(filesToModify);
  
  // Now inject documentation into each file
  for (const [extensionFile, config] of Object.entries(EXTENSION_CONFIG)) {
    const extensionPath = path.join(EXTENSIONS_DIR, extensionFile);
    const functionDocs = extractFunctionDocs(extensionPath);

    for (const targetFile of config.targets) {
      const targetPath = path.join(__dirname, '..', targetFile);
      
      if (fs.existsSync(targetPath)) {
        // Inject documentation and write back to original location
        const injectedContent = injectDocumentation(targetPath, config, functionDocs);
        fs.writeFileSync(targetPath, injectedContent);
        
        console.log(`    ✓ Injected documentation into ${targetFile}`);
      }
    }
  }
}

/**
 * Main execution function
 */
function main() {
  console.log('Documentation Injection Script');
  console.log('=============================\n');

  try {
    // Inject documentation into source files
    injectDocumentationInPlace();
    
    console.log('\n✅ Documentation injection completed successfully!');
    console.log('Source files have been temporarily modified with injected documentation.');
    console.log('Build documentation now, then run this script with --cleanup to restore originals.');
    
  } catch (error) {
    console.error('❌ Error during documentation injection:', error.message);
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
  extractFunctionDocs,
  injectDocumentation,
  injectDocumentationInPlace,
  cleanup,
  restoreBackups,
  BACKUP_DIR,
  EXTENSION_CONFIG
};