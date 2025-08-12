#!/usr/bin/env node

/**
 * Documentation verification tests for extension module inheritance
 * 
 * This test suite verifies that all functions from extension modules
 * have proper documentation when included in target modules.
 * 
 * These tests follow TDD principles and should FAIL initially until
 * the documentation injection solution is implemented.
 */

const fs = require('fs');
const path = require('path');

// Configuration for expected functions from Foldable extensions
const FOLDABLE_FUNCTIONS = [
  'any', 'all', 'containsBy', 'contains', 'indexOfBy', 'indexOf',
  'minBy', 'min', 'maxBy', 'max', 'countBy', 'length', 'size', 'count',
  'forEach', 'forEachWithIndex', 'find', 'findWithIndex', 'toList', 'foldMap'
];

// Modules that include Foldable extensions
const TARGET_MODULES = [
  'List', 'Array', 'Option', 'Result', 'Tree', 'NonEmpty', 'SequenceZipper'
];

/**
 * Checks if a generated HTML documentation file contains documentation
 * for the specified function
 */
function hasFunctionDocumentation(htmlContent, functionName) {
  // Look for function definition with documentation
  const functionPattern = new RegExp(
    `<div[^>]*id="val-${functionName}"[^>]*>.*?</div>.*?<div[^>]*class="spec-doc"[^>]*>.*?<p>.*?</p>`,
    'si'
  );
  
  return functionPattern.test(htmlContent);
}

/**
 * Reads generated HTML documentation for a module
 */
function readModuleDocumentation(moduleName) {
  const docPath = path.join(__dirname, '..', '_build', 'default', '_doc', '_html', 'relude', 'Relude', 'Globals', moduleName, 'index.html');
  
  if (!fs.existsSync(docPath)) {
    throw new Error(`Documentation file not found: ${docPath}`);
  }
  
  return fs.readFileSync(docPath, 'utf8');
}

/**
 * Test that verifies all Foldable extension functions have documentation
 */
function testFoldableDocumentation() {
  let totalTests = 0;
  let passedTests = 0;
  let failedTests = [];

  console.log('Testing Foldable extension function documentation...\n');

  for (const moduleName of TARGET_MODULES) {
    console.log(`Testing ${moduleName} module:`);
    
    try {
      const htmlContent = readModuleDocumentation(moduleName);
      
      for (const functionName of FOLDABLE_FUNCTIONS) {
        totalTests++;
        const hasDoc = hasFunctionDocumentation(htmlContent, functionName);
        
        if (hasDoc) {
          console.log(`  ✓ ${functionName} has documentation`);
          passedTests++;
        } else {
          console.log(`  ✗ ${functionName} missing documentation`);
          failedTests.push(`${moduleName}.${functionName}`);
        }
      }
      
    } catch (error) {
      console.log(`  ✗ Error reading ${moduleName} documentation: ${error.message}`);
      // Mark all functions as failed for this module
      for (const functionName of FOLDABLE_FUNCTIONS) {
        totalTests++;
        failedTests.push(`${moduleName}.${functionName}`);
      }
    }
    
    console.log('');
  }

  return { totalTests, passedTests, failedTests };
}

/**
 * Test that verifies extension module source files have documentation
 */
function testExtensionSourceDocumentation() {
  console.log('Testing extension module source documentation...\n');
  
  const extensionPath = path.join(__dirname, '..', 'src', 'extensions', 'Relude_Extensions_Foldable.re');
  
  if (!fs.existsSync(extensionPath)) {
    throw new Error(`Extension source file not found: ${extensionPath}`);
  }
  
  const sourceContent = fs.readFileSync(extensionPath, 'utf8');
  let foundDocs = 0;
  
  for (const functionName of FOLDABLE_FUNCTIONS) {
    // Look for function documentation in ReasonML format
    const docPattern = new RegExp(`/\\*\\*[\\s\\S]*?\\*/[\\s\\S]*?let\\s+${functionName}\\s*:`, 'g');
    
    if (docPattern.test(sourceContent)) {
      console.log(`  ✓ ${functionName} has source documentation`);
      foundDocs++;
    } else {
      console.log(`  ✗ ${functionName} missing source documentation`);
    }
  }
  
  console.log(`\nFound documentation for ${foundDocs}/${FOLDABLE_FUNCTIONS.length} functions\n`);
  
  return foundDocs === FOLDABLE_FUNCTIONS.length;
}

/**
 * Main test runner
 */
function runTests() {
  console.log('Documentation Verification Tests');
  console.log('================================\n');
  
  try {
    // First verify extension sources have documentation
    const sourceDocsOk = testExtensionSourceDocumentation();
    
    if (!sourceDocsOk) {
      console.log('❌ Extension source documentation is incomplete');
      process.exit(1);
    }
    
    // Then test that target module documentation exists
    const { totalTests, passedTests, failedTests } = testFoldableDocumentation();
    
    console.log('Results:');
    console.log('========');
    console.log(`Total tests: ${totalTests}`);
    console.log(`Passed: ${passedTests}`);
    console.log(`Failed: ${failedTests.length}`);
    
    if (failedTests.length > 0) {
      console.log('\nFailed tests:');
      failedTests.forEach(test => console.log(`  - ${test}`));
    }
    
    console.log(`\nSuccess rate: ${((passedTests / totalTests) * 100).toFixed(1)}%`);
    
    if (failedTests.length === 0) {
      console.log('\n✅ All documentation tests passed!');
      process.exit(0);
    } else {
      console.log('\n❌ Documentation tests failed - missing function documentation');
      console.log('\nThis is expected before implementing the documentation injection solution.');
      process.exit(1);
    }
    
  } catch (error) {
    console.error('Error running tests:', error.message);
    process.exit(1);
  }
}

// Run tests if this script is executed directly
if (require.main === module) {
  runTests();
}

module.exports = {
  testFoldableDocumentation,
  testExtensionSourceDocumentation,
  hasFunctionDocumentation,
  FOLDABLE_FUNCTIONS,
  TARGET_MODULES
};