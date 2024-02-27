export type CommonErrorPageProps = {
  tittle: number;
  description: string;
  tittleClassName: string;
  BtnClassName: string;
};

export type CommonCardHeadingPropsType = {
  bigHeadingClassName?: string;
  Heading?: string;
  smallHeading?: string;
  span?: string;
  headingClassName?: string;
  span2?: string;
};

export type Upload = {
  filename: string;
  mimetype: string;
  encoding: string;
};

export type PaginatedObject<T> = {
  data: T[];
  meta: {
    total: number;
    lastPage: number;
    currentPage: number;
    perPage: number;
    prev: number | null;
    next: number | null;
  };
};

export type WhereAndOrderInput = {
  where?: FindManyInput;
  orderBy?: OrderByInput;
};

export type WhereInput = {
  where?: FindManyInput;
};

export type OrderInput = {
  orderBy?: OrderByInput;
};

export type FindManyInput = {
  [key: string]: any;
};

export type OrderByInput = {
  [key: string]: "asc" | "desc";
};

export type PaginatedInput = {
  page: number;
  perPage: number;
  where?: FindManyInput;
  orderBy?: OrderByInput;
};

export type FindBySlugInput = {
  slug: string;
};

export type FindByIdInput = {
  id: string;
};

export type FindByIdsInput = {
  ids: string[];
};
