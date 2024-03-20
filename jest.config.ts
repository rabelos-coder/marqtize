import type { Config } from 'jest'
import nextJest from 'next/jest'

// Sync object
const createJestConfig = nextJest({
  // Provide the path to your Next.js app to load next.config.js and .env files in your test environment
  dir: './',
})

// Add any custom config to be passed to Jest
const customJestConfig: Config = {
  // clearMocks: true,
  // collectCoverage: true,
  // coverageDirectory: 'coverage',
  // collectCoverageFrom: [
  //   'src/components/**/*.ts(x)?',
  //   'src/templates/**/*.ts(x)?',
  //   '!src/**/stories.tsx',
  // ],
  setupFilesAfterEnv: ['<rootDir>/jest.setup.js'],
  testEnvironment: 'jest-environment-jsdom',
  moduleDirectories: ['node_modules', '<rootDir>'],
  testPathIgnorePatterns: [
    '<rootDir>/.next/',
    '<rootDir>/node_modules/',
    '<rootDir>/cypress/',
  ],
  preset: 'ts-jest',
  modulePaths: ['<rootDir>/src/'],
  moduleNameMapper: {
    '^@/(.*)$': '<rootDir>/src$1',
  },
}

export default createJestConfig(customJestConfig)
