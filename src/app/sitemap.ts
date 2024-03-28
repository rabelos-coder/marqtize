import * as Sentry from '@sentry/nextjs'
import { MetadataRoute } from 'next'
import { headers } from 'next/headers'
import { join } from 'path'

import { FIND_MANY_CATEGORIES } from '@/graphql/blogCategory'
import { FIND_MANY_POSTS } from '@/graphql/blogPost'
import { FIND_MANY_TAGS } from '@/graphql/blogTags'
import { OrderByEnum } from '@/types/common'
import { recursiveScanDir } from '@/utils'
import { createApolloClient } from '@/utils/apollo'

const ignore = [
  '.DS_Store',
  '[...rest]',
  'auth',
  'backend',
  'scan',
  'basic',
  'blog',
  'errors',
  'sentry-example-page',
  'sub-domains',
  'not-found.tsx',
  'error.tsx',
  'layout.tsx',
  'service-unavailable.tsx',
  'unauthorized.tsx',
  'bad-request.tsx',
]

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const header = headers()
  const host = header.get('host') ?? ''
  const protocol = header.get('x-forwarded-proto') ?? 'http'
  const baseUrl = `${protocol}://${host}`
  const client = createApolloClient()

  const blogRoutes: any[] = []
  const [categories, tags, posts] = await Promise.all([
    // CATEGORIES
    client.query({
      query: FIND_MANY_CATEGORIES,
      variables: {
        where: {
          deletedAt: null,
        },
        orderBy: {
          name: OrderByEnum.ASC,
        },
      },
    }),

    // TAGS
    client.query({
      query: FIND_MANY_TAGS,
      variables: {
        where: {
          deletedAt: null,
        },
        orderBy: {
          name: OrderByEnum.ASC,
        },
      },
    }),

    // POSTS
    client.query({
      query: FIND_MANY_POSTS,
      variables: {
        where: {
          AND: [
            {
              publishedAt: {
                lte: new Date().toISOString(),
              },
            },
          ],
          isPublished: true,
          deletedAt: null,
        },
        orderBy: {
          publishedAt: OrderByEnum.DESC,
        },
      },
    }),
  ])

  if (categories.error) {
    Sentry.captureException(categories.error)
    console.log(categories.error?.message)
  }

  if (tags.error) {
    Sentry.captureException(tags.error)
    console.log(tags.error?.message)
  }

  if (posts.error) {
    Sentry.captureException(posts.error)
    console.log(posts.error?.message)
  }

  if (categories.data?.findManyBlogCategory?.length) {
    blogRoutes.push(
      ...categories.data.findManyBlogCategory.map((category) => ({
        url: `${baseUrl}/blog/category/${category.slug}`,
        lastModified: new Date(category.updatedAt),
        changeFrequency: 'monthly',
        priority: 0.7,
      }))
    )
  }

  if (tags.data?.findManyBlogTag?.length) {
    blogRoutes.push(
      ...tags.data.findManyBlogTag.map((tag) => ({
        url: `${baseUrl}/blog/tag/${tag.slug}`,
        lastModified: new Date(tag.updatedAt),
        changeFrequency: 'monthly',
        priority: 0.7,
      }))
    )
  }

  if (posts.data?.findManyBlogPost?.length) {
    blogRoutes.push(
      ...posts.data.findManyBlogPost.map((post) => ({
        url: `${baseUrl}/blog/post/${post.slug}`,
        lastModified: new Date(post.updatedAt),
        changeFrequency: 'monthly',
        priority: 0.7,
      }))
    )
  }

  const basePath = join(process.cwd(), 'src/app/[locale]')

  const dynamicRoutes = recursiveScanDir(basePath)
    .filter(
      (item) =>
        !ignore.includes(item.name) &&
        !ignore.some((ignoreItem) => item.path.includes(ignoreItem))
    )
    .map((item) => {
      return {
        url:
          baseUrl +
          item.path
            .replace(basePath, '')
            .replace('/page.jsx', '')
            .replace('/page.js', '')
            .replace('/page.tsx', '')
            .replace('/page.ts', ''),
        lastModified: new Date(),
        changeFrequency: 'yearly',
        priority: 0.8,
      }
    })

  return [
    {
      url: `${baseUrl}/`,
      lastModified: new Date(),
      changeFrequency: 'yearly',
      priority: 1,
    },
    {
      url: `${baseUrl}/blog`,
      lastModified: new Date(),
      changeFrequency: 'daily',
      priority: 0.8,
    },
    ...blogRoutes,
    ...dynamicRoutes,
  ]
}
