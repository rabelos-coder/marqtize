import { gql, TypedDocumentNode } from "@apollo/client";

import { WhereAndOrderInput } from "@/types/common";
import { FindManyTimezone } from "@/types/timezone";

const FRAGMENT_TIMEZONE_PROPS = gql`
  fragment TimezoneProps on Timezone {
    id
    name
    code
    createdAt
    updatedAt
  }
`;

export const FIND_MANY_TIMEZONES: TypedDocumentNode<
  FindManyTimezone,
  WhereAndOrderInput
> = gql`
  ${FRAGMENT_TIMEZONE_PROPS}
  query FindManyTimezone(
    $where: SearchTimezoneInput
    $orderBy: SortTimezoneInput
  ) {
    findManyTimezone(where: $where, orderBy: $orderBy) {
      ...TimezoneProps
    }
  }
`;
