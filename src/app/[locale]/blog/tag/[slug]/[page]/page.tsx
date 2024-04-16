import { notFound } from 'next/navigation'
import { getTranslations } from 'next-intl/server'
import { Container } from 'reactstrap'

import SvgBorder from '@/components/frontend/common/SvgBorder'
import { PAGINATED_POSTS } from '@/graphql/blogPost'
import { FIND_FIRST_TAG } from '@/graphql/blogTags'
import Header from '@/layouts/frontend/landing/Header'
import LandingLayout from '@/layouts/frontend/landing/LandingLayout'
import { Link } from '@/navigation'
import { ChildrenWithParamsProps, OrderByEnum } from '@/types/common'
import { createApolloClient } from '@/utils/apollo'
import { concatTitle } from '@/utils/helpers'
import { Posts } from '@/views/frontend/landing/Posts'

const client = createApolloClient()

export async function generateMetadata({
  params: { locale, slug },
}: ChildrenWithParamsProps) {
  const t = await getTranslations({ locale })
  let title = concatTitle(t('blog.tags.title'))

  const { data, error } = await client.query({
    query: FIND_FIRST_TAG,
    variables: {
      where: {
        slug,
        deletedAt: null,
      },
    },
  })

  if (error) throw new Error(error?.message)

  if (data?.findFirstBlogTag)
    title = concatTitle(
      t('blog.tags.title2', { tag: data.findFirstBlogTag.name })
    )

  return {
    title,
  }
}

export default async function PostsByTagPage({
  params: { locale, slug, page },
}: ChildrenWithParamsProps) {
  const t = await getTranslations({ locale })

  page = parseInt(`${page}`)

  const tag = await client.query({
    query: FIND_FIRST_TAG,
    variables: {
      where: {
        slug,
        deletedAt: null,
      },
    },
  })

  if (!tag || tag.error) throw new Error(tag.error?.message)

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
        tags: {
          some: {
            tag: {
              id: tag.data.findFirstBlogTag.id,
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

  const { findFirstBlogTag } = tag.data
  const { paginatedBlogPost } = posts.data

  if (
    page <= 0 ||
    (paginatedBlogPost?.meta?.lastPage &&
      paginatedBlogPost.meta.lastPage < page) ||
    !findFirstBlogTag
  ) {
    notFound()
  }

  return (
    <LandingLayout>
      <Header
        title={t('blog.tags.title2', { tag: findFirstBlogTag.name })}
        description={t('blog.tags.shortDescription')}
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
