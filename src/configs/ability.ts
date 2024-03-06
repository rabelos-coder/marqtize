import { AbilityBuilder, PureAbility } from "@casl/ability";

import { Action, ActionEnum } from "@/types/action";
import { AllSubjectsEnum, PublicSubjectEnum, Subject } from "@/types/subject";
import { User } from "@/types/user";

export type AppAbility = PureAbility<[Action, string]> | undefined;

export const AppAbility = PureAbility as any;

export type AclAbility = {
  action: Action;
  subject: Subject;
};

// eslint-disable-next-line @typescript-eslint/no-unused-vars, unused-imports/no-unused-vars
function defineRulesFor(user: User) {
  const { can, rules } = new AbilityBuilder(AppAbility);

  const isAdmin = user.roles?.map((role) => role.slug)?.includes("admin");

  can(ActionEnum.Read, PublicSubjectEnum.All);

  if (isAdmin) {
    for (const action of Object.values(ActionEnum)) {
      for (const subject of Object.values(PublicSubjectEnum)) {
        can(action, subject);
      }
    }
  } else if (user.isSuperAdmin) {
    for (const action of Object.values(ActionEnum)) {
      for (const subject of Object.values(AllSubjectsEnum)) {
        can(action, subject);
      }
    }
  } else if (!isAdmin || !user.isSuperAdmin) {
    for (const claim of user.claims) {
      const [subject, action] = claim.split(":");
      can(action, subject);
    }
  }

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
  action: ActionEnum.Read,
  subject: "All",
};

export default defineRulesFor;
