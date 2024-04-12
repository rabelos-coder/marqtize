import { withSentryConfig } from '@sentry/nextjs'
import createNextIntlPlugin from 'next-intl/plugin'
import nextPWA from 'next-pwa'
import { join } from 'path'

const withNextIntl = createNextIntlPlugin()

const IS_DEVELOPMENT = process.env.NODE_ENV === 'development'

const withPWA = nextPWA({
  dest: 'public',
  disable: IS_DEVELOPMENT,
})

/** @type {import('next').NextConfig} */
const nextConfig = {
  poweredByHeader: false,
  generateEtags: false,
  reactStrictMode: true,
  swcMinify: true,
  distDir: 'build',
  images: {
    remotePatterns: [
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
    ],
  },
}

if (IS_DEVELOPMENT) {
  nextConfig['sassOptions'] = {
    includePaths: [join(process.cwd(), 'src/app/scss')],
    sourceMap: true,
  }
  nextConfig['webpack'] = (config) => {
    /**
     * Force scss source maps for debugging. If there are performance issues or you don't need debug css, use the value "eval-source-map" instead.
     */
    Object.defineProperty(config, 'devtool', {
      get() {
        return 'source-map'
      },
      set() {},
    })

    return config
  }
}

export default IS_DEVELOPMENT
  ? withNextIntl(withPWA(nextConfig))
  : withSentryConfig(
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
