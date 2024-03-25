import { notFound } from 'next/navigation'
import { getTranslations } from 'next-intl/server'
import { Container } from 'reactstrap'

import { SvgBorder } from '@/components/frontend/common/SvgBorder'
import { FIND_FIRST_CATEGORY } from '@/graphql/blogCategory'
import { PAGINATED_POSTS } from '@/graphql/blogPost'
import { Header } from '@/layout/frontend/landing/Header'
import { LandingLayout } from '@/layout/frontend/landing/LandingLayout'
import { Link } from '@/navigation'
import { OrderByEnum } from '@/types/common'
import { createApolloClient } from '@/utils/apollo'
import { concatTitle } from '@/utils/helpers'
import { Posts } from '@/views/frontend/landing/Posts'

const client = createApolloClient()

export async function generateMetadata({ params: { locale, slug } }: any) {
  const t = await getTranslations({ locale })
  let title = concatTitle(t('blog.categories.title'))

  const { data, error } = await client.query({
    query: FIND_FIRST_CATEGORY,
    variables: {
      where: {
        slug,
        deletedAt: null,
      },
    },
  })

  if (error) throw new Error(error?.message)

  if (data?.findFirstBlogCategory)
    title = concatTitle(
      t('blog.categories.title2', { category: data.findFirstBlogCategory.name })
    )

  return {
    title,
  }
}

export default async function PostsByCategoryPage({
  params: { locale, slug, page },
}: any) {
  const t = await getTranslations({ locale })

  page = parseInt(`${page}`)

  const category = await client.query({
    query: FIND_FIRST_CATEGORY,
    variables: {
      where: {
        slug,
        deletedAt: null,
      },
    },
  })

  if (!category || category.error) throw new Error(category.error?.message)

  const posts = await client.query({
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
        categories: {
          some: {
            category: {
              id: category.data.findFirstBlogCategory.id,
            },
          },
        },
        isPublished: true,
        deletedAt: null,
      },
      orderBy: {
        publishedAt: OrderByEnum.DESC,
      },
    },
  })

  if (!posts || posts.error) throw new Error(posts.error?.message)

  const { findFirstBlogCategory } = category.data
  const { paginatedBlogPost } = posts.data

  if (
    page <= 0 ||
    (paginatedBlogPost?.meta?.lastPage &&
      paginatedBlogPost.meta.lastPage < page) ||
    !findFirstBlogCategory
  ) {
    notFound()
  }

  return (
    <LandingLayout>
      <Header
        title={t('blog.categories.title2', {
          category: findFirstBlogCategory.name,
        })}
        description={t('blog.categories.shortDescription')}
      />
      <section className="bg-light py-10">
        <Posts
          posts={paginatedBlogPost?.data ?? []}
          meta={paginatedBlogPost?.meta ?? null}
        />
        <Container>
          <hr className="my-3" />
          <div className="text-center">
            <Link className="btn btn-transparent-dark" href="/blog/1">
              {t('backToBlog')}
            </Link>
          </div>
        </Container>
        <SvgBorder className="text-dark" />
      </section>
    </LandingLayout>
  )
}
