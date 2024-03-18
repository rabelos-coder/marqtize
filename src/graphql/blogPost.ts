import { gql, TypedDocumentNode } from "@apollo/client";

import { FindFirstBlogPost, PaginatedBlogPost } from "@/types/blog";
import { PaginatedInput, WhereAndOrderInput } from "@/types/common";

const FRAGMENT_BLOG_POST_PROPS = gql`
  fragment BlogPostProps on BlogPost {
    id
    slug
    title
    resume
    content
    coverImage
    isPublished
    publishedAt
    createdAt
    updatedAt
    deletedAt
    author {
      id
      name
      image
    }
    tags {
      id
      name
      slug
    }
    categories {
      id
      name
      slug
    }
  }
`;

export const PAGINATED_POSTS: TypedDocumentNode<
  PaginatedBlogPost,
  PaginatedInput
> = gql`
  ${FRAGMENT_BLOG_POST_PROPS}
  query PaginatedBlogPost(
    $page: Int!
    $perPage: Int!
    $where: SearchBlogPostInput
    $orderBy: SortBlogPostInput
  ) {
    paginatedBlogPost(
      page: $page
      perPage: $perPage
      where: $where
      orderBy: $orderBy
    ) {
      data {
        ...BlogPostProps
      }
      meta {
        total
        lastPage
        currentPage
        perPage
        prev
        next
      }
    }
  }
`;

export const FIND_FIRST_POST: TypedDocumentNode<
  FindFirstBlogPost,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_BLOG_POST_PROPS}
  query FindFirstBlogPost(
    $where: SearchBlogPostInput
    $orderBy: SortBlogPostInput
  ) {
    findFirstBlogPost(where: $where, orderBy: $orderBy) {
      ...BlogPostProps
    }
  }
`;
