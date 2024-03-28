import { gql, TypedDocumentNode } from '@apollo/client'

import { FindFirstBlogCategory, FindManyBlogCategory } from '@/types/blog'
import { WhereAndOrderInput } from '@/types/common'

const FRAGMENT_BLOG_CATEGORY_PROPS = gql`
  fragment BlogCategoryProps on BlogCategory {
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

export const FIND_MANY_CATEGORIES: TypedDocumentNode<
  FindManyBlogCategory,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_BLOG_CATEGORY_PROPS}
  query FindManyBlogCategory(
    $where: SearchBlogCategoryInput
    $orderBy: SortBlogCategoryInput
  ) {
    findManyBlogCategory(where: $where, orderBy: $orderBy) {
      ...BlogCategoryProps
    }
  }
`

export const FIND_FIRST_CATEGORY: TypedDocumentNode<
  FindFirstBlogCategory,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_BLOG_CATEGORY_PROPS}
  query FindFirstBlogCategory(
    $where: SearchBlogCategoryInput
    $orderBy: SortBlogCategoryInput
  ) {
    findFirstBlogCategory(where: $where, orderBy: $orderBy) {
      ...BlogCategoryProps
    }
  }
`
