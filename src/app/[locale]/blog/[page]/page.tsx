import { notFound } from 'next/navigation'
import { getTranslations } from 'next-intl/server'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { PAGINATED_POSTS } from '@/graphql/blogPost'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { OrderByEnum } from '@/types/common'
import { createApolloClient } from '@/utils/apollo'
import { concatTitle } from '@/utils/helpers'
import { Posts } from '@/views/frontend/landing/Posts'

const client = createApolloClient()

export async function generateMetadata({ params: { locale } }: any) {
  const t = await getTranslations({ locale })
  const title = concatTitle(t('blog.title'))

  return {
    title,
  }
}

export default async function PostsPage({ params: { locale, page } }: any) {
  const t = await getTranslations({ locale })

  page = parseInt(`${page}`)

  const { data, error } = await client.query({
    query: PAGINATED_POSTS,
    variables: {
      page,
      perPage: 6,
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
  })

  if (error) throw new Error(error?.message)

  const { paginatedBlogPost } = data

  if (
    page <= 0 ||
    (paginatedBlogPost?.meta?.lastPage &&
      paginatedBlogPost.meta.lastPage < page)
  ) {
    notFound()
  }

  return (
    <LandingLayout>
      <Header
        title={t('blog.title')}
        description={t('blog.shortDescription')}
      />
      <section className="bg-light py-10">
        <Posts
          posts={paginatedBlogPost?.data ?? []}
          meta={paginatedBlogPost?.meta ?? null}
        />
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
