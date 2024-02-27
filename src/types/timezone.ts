export type Timezone = {
  id: number;
  code: string;
  name: string;
};

export type FindManyTimezone = {
  findManyTimezone: Timezone[];
};
