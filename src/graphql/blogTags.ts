import { gql, TypedDocumentNode } from "@apollo/client";

import { FindFirstBlogTag } from "@/types/blog";
import { WhereAndOrderInput } from "@/types/common";

const FRAGMENT_BLOG_TAG_PROPS = gql`
  fragment BlogTagProps on BlogTag {
    name
    slug
    posts {
      slug
      title
      isPublished
      publishedAt
      categories {
        name
        slug
      }
    }
  }
`;

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
`;
