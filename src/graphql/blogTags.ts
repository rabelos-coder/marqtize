import { gql, TypedDocumentNode } from "@apollo/client";

import { FindFirstBlogTag } from "@/types/blog";
import { WhereAndOrderInput } from "@/types/common";

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
