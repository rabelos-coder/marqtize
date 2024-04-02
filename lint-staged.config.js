module.exports = {
  // Type check TypeScript files
  './src/**/*.(ts|tsx)': () => 'yarn tsc --noEmit',

  // Lint then format TypeScript and JavaScript files
  './src/**/*.(ts|tsx|js)': () => [`yarn lint:fix`, `yarn format:fix`],

  // Format MarkDown and JSON
  '**/*.(md|json)': (filenames) =>
    `yarn prettier --write ${filenames.join(' ')}`,
}
