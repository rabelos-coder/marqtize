import { AbilityBuilder, PureAbility } from '@casl/ability'

import { Action, ActionEnum } from '@/types/action'
import { JWT } from '@/types/jwt'
import { PublicSubjectsEnum, Subject, Subjects } from '@/types/subject'

export type AppAbility = PureAbility<[Action, string]> | undefined

export const AppAbility = PureAbility as any

export type AclAbility = {
  action: Action
  subject: Subject | keyof typeof Subjects
}

// eslint-disable-next-line @typescript-eslint/no-unused-vars, unused-imports/no-unused-vars
function defineRulesFor(jwt: JWT) {
  const { can, rules } = new AbilityBuilder(AppAbility)

  const isAdmin = jwt.roles.includes('admin')

  can(ActionEnum.Manage, Subjects.All)

  if (isAdmin) {
    for (const action of Object.values(ActionEnum)) {
      for (const subject of Object.values(PublicSubjectsEnum)) {
        can(action, subject)
      }
    }
  } else if (jwt.sa) {
    for (const action of Object.values(ActionEnum)) {
      for (const subject of Object.values(Subjects)) {
        can(action, subject)
      }
    }
  } else if (!isAdmin || !jwt.sa) {
    for (const claim of jwt.claims) {
      const [subject, action] = claim.split(':')
      can(action, subject)
    }
  }

  return rules
}

export const buildAbilityFor = (jwt: JWT): AppAbility => {
  return new AppAbility(defineRulesFor(jwt), {
    // https://casl.js.org/v5/en/guide/subject-type-detection
    // @ts-ignore
    detectSubjectType: (object) => object!.type,
  })
}

export const defaultAcl: AclAbility = {
  action: ActionEnum.Manage,
  subject: Subjects.All,
}

export default defineRulesFor
