import { gql, TypedDocumentNode } from "@apollo/client";

import { FindFirstBlogCategory } from "@/types/blog";
import { WhereAndOrderInput } from "@/types/common";

const FRAGMENT_BLOG_CATEGORY_PROPS = gql`
  fragment BlogCategoryProps on BlogCategory {
    name
    slug
    posts {
      slug
      title
      isPublished
      publishedAt
      tags {
        name
        slug
      }
      categories {
        name
        slug
      }
    }
  }
`;

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
`;
