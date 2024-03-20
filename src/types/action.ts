export enum ActionEnum {
  Create = 'Create',
  Read = 'Read',
  Update = 'Update',
  Delete = 'Delete',
}

export type Action = keyof typeof ActionEnum
