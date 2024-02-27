import { AbilityBuilder, PureAbility } from "@casl/ability";

import { User } from "@/types/user";

type Actions = "manage" | "create" | "read" | "update" | "delete";

export type AppAbility = PureAbility<[Actions, string]> | undefined;

export const AppAbility = PureAbility as any;

export type AclAbility = {
  action: Actions;
  subject: string;
};

// eslint-disable-next-line @typescript-eslint/no-unused-vars, unused-imports/no-unused-vars
function defineRulesFor(user: User) {
  const { can, rules } = new AbilityBuilder(AppAbility);

  can("manage", "all");
  can("create", "profile");

  // if (user.role === "admin") {
  //   can("manage", "all");
  // }
  // if (user.role === "manager") {
  //   can("manage", "article");
  // }

  return rules;
}

export const buildAbilityFor = (user: User): AppAbility => {
  return new AppAbility(defineRulesFor(user), {
    // https://casl.js.org/v5/en/guide/subject-type-detection
    // @ts-ignore
    detectSubjectType: (object) => object!.type,
  });
};

export const defaultAcl: AclAbility = {
  action: "manage",
  subject: "all",
};

export default defineRulesFor;
