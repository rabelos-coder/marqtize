module.exports = {
  // Format MarkDown and JSON
  './src/**/*.(ts|tsx|js|jsx)': () => `bun format:fix`,

  // Type check TypeScript files
  './src/**/*.(ts|tsx)': () => 'bun tsc --noEmit',

  // Lint then format TypeScript and JavaScript files
  './src/**/*.(ts|tsx|js|jsx)': () => [`bun lint:fix`, `bun format:fix`],

  // Format MarkDown and JSON
  '**/*.(md|json)': (filenames) =>
    `bun prettier --write ${filenames.join(' ')}`,
}
