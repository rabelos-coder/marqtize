import { gql, TypedDocumentNode } from '@apollo/client'

import { FindFirstBlogTag, FindManyBlogTag } from '@/types/blog'
import { WhereAndOrderInput } from '@/types/common'

const FRAGMENT_BLOG_TAG_PROPS = gql`
  fragment BlogTagProps on BlogTag {
    id
    name
    slug
    createdAt
    updatedAt
    deletedAt
    posts {
      id
      slug
      title
    }
  }
`

export const FIND_MANY_TAGS: TypedDocumentNode<
  FindManyBlogTag,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_BLOG_TAG_PROPS}
  query FindManyBlogTag(
    $where: SearchBlogTagInput
    $orderBy: SortBlogTagInput
  ) {
    findManyBlogTag(where: $where, orderBy: $orderBy) {
      ...BlogTagProps
    }
  }
`

export const FIND_FIRST_TAG: TypedDocumentNode<
  FindFirstBlogTag,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_BLOG_TAG_PROPS}
  query FindFirstBlogTag(
    $where: SearchBlogTagInput
    $orderBy: SortBlogTagInput
  ) {
    findFirstBlogTag(where: $where, orderBy: $orderBy) {
      ...BlogTagProps
    }
  }
`
