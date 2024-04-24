module.exports = {
  // Type check TypeScript files
  './src/**/*.(ts|tsx)': () => 'bun tsc --noEmit',

  // Format TypeScript and JavaScript files
  './src/**/*.(ts|tsx|js|jsx)': () => `bun run format:fix`,

  // Lint then format TypeScript and JavaScript files
  './src/**/*.(ts|tsx|js|jsx)': () => [
    `bun run lint:fix`,
    `bun run format:fix`,
  ],

  // Format MarkDown and JSON
  '**/*.(md|json)': (filenames) =>
    `bunx prettier --write ${filenames.join(' ')}`,
}
