import { AclAbility } from "@/configs/ability";
import { ReactNode } from "react";

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
