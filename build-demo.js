// Build script to bundle PureScript demo for the browser
const esbuild = require('esbuild');
const fs = require('fs');
const path = require('path');

// Create entry point that imports and runs the main function
const entryPointContent = `
import * as WebDemo from './output/ECS.Examples.WebDemo/index.js';

export const main = WebDemo.main;
`;

// Write entry point
fs.writeFileSync('demo-entry.js', entryPointContent);

// Bundle with esbuild
esbuild.build({
  entryPoints: ['demo-entry.js'],
  bundle: true,
  format: 'esm',
  outfile: 'docs/app.js',
  platform: 'browser',
  target: 'es2020',
  minify: true,
}).then(() => {
  console.log('✓ Demo built successfully to docs/app.js');
  // Clean up entry point
  fs.unlinkSync('demo-entry.js');
}).catch((error) => {
  console.error('Build failed:', error);
  process.exit(1);
});
