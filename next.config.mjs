import bundleAnalyzer from '@next/bundle-analyzer'
import { withSentryConfig } from '@sentry/nextjs'
import createNextIntlPlugin from 'next-intl/plugin'
import nextPWA from 'next-pwa'
import { join } from 'path'

/**
 * Environment constants
 */
const ENVIRONMENT = process.env.NODE_ENV ?? 'development'
const IS_PRODUCTION = ENVIRONMENT === 'production'
const IS_DEVELOPMENT = ENVIRONMENT === 'development'

/**
 * Next Intl Config
 */
const withNextIntl = createNextIntlPlugin()

/**
 * Bundle Analyzer Config
 */
const withBundleAnalyzer = bundleAnalyzer({
  enabled: process.env.ANALYZE === 'true',
  openAnalyzer: true,
})

/**
 * Progressive Web APP Config
 */
const withPWA = nextPWA({
  dest: 'public',
  maximumFileSizeToCacheInBytes: 5000000,
})

/**
 * Remove Images Patterns
 */
/** @type {import('next/dist/shared/lib/image-config').RemotePattern[]} */
const remotePatterns = [
  {
    protocol: 'http',
    hostname: 'localhost',
    port: '9001',
  },
  {
    protocol: 'http',
    hostname: 'localhost',
    port: '3000',
  },
  {
    protocol: 'http',
    hostname: 'localhost',
    port: '4000',
  },
  {
    protocol: 'https',
    hostname: 'lh3.googleusercontent.com',
  },
  {
    protocol: 'https',
    hostname: 'scontent.faqa2-1.fna.fbcdn.net',
  },
  {
    protocol: 'https',
    hostname: 'ui-avatars.com',
  },
]

/**
 * Next.js Config
 */
/** @type {import('next').NextConfig} */
const nextConfig = {
  poweredByHeader: IS_PRODUCTION,
  productionBrowserSourceMaps: false,
  reactStrictMode: true,
  images: {
    remotePatterns,
  },
  sassOptions: {
    sourceMap: IS_DEVELOPMENT,
    includePaths: [join(process.cwd(), 'src/assets/scss')],
  },
  webpack: (config, { dev }) => {
    if (config.cache && !dev) {
      config.cache = Object.freeze({
        type: 'memory',
      })
      config.cache.maxMemoryGenerations = 0
      config.experimental.serverSourceMaps = false
    }

    if (IS_DEVELOPMENT) {
      Object.defineProperty(config, 'devtool', {
        get() {
          return 'source-map'
        },
        set() {},
      })
    }

    return config
  },
}

export default IS_DEVELOPMENT
  ? withNextIntl(nextConfig)
  : withBundleAnalyzer(
      withSentryConfig(
        withNextIntl(withPWA(nextConfig)),
        {
          // For all available options, see:
          // https://github.com/getsentry/sentry-webpack-plugin#options

          // Suppresses source map uploading logs during build
          silent: true,
          org: 'rabelos-coder',
          project: 'marqtize-web',
        },
        {
          // For all available options, see:
          // https://docs.sentry.io/platforms/javascript/guides/nextjs/manual-setup/

          // Upload a larger set of source maps for prettier stack traces (increases build time)
          widenClientFileUpload: true,

          // Transpiles SDK to be compatible with IE11 (increases bundle size)
          transpileClientSDK: true,

          // Routes browser requests to Sentry through a Next.js rewrite to circumvent ad-blockers. (increases server load)
          // Note: Check that the configured route will not match with your Next.js middleware, otherwise reporting of client-
          // side errors will fail.
          tunnelRoute: '/monitoring',

          // Hides source maps from generated client bundles
          hideSourceMaps: true,

          // Automatically tree-shake Sentry logger statements to reduce bundle size
          disableLogger: true,

          // Enables automatic instrumentation of Vercel Cron Monitors.
          // See the following for more information:
          // https://docs.sentry.io/product/crons/
          // https://vercel.com/docs/cron-jobs
          automaticVercelMonitors: true,
        }
      )
    )
