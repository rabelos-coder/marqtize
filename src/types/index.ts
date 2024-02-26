import { ReactNode } from "react";

import { AclAbility } from "@/configs/ability";

type ComponentParams = {
  id?: string;
  slug?: string;
  locale?: string;
  acl?: AclAbility;
};

export type ComponentProps = Readonly<{
  children?: ReactNode;
  params?: ComponentParams;
}>;
