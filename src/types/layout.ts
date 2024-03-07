import { ChangeEvent, Dispatch, ReactNode, SetStateAction } from "react";

export type LayoutContextType = {
  searchIcon: boolean;
  bookMarkClass: boolean;
  setBookMarkClass: Dispatch<SetStateAction<boolean>>;
  setSearchIcon: Dispatch<SetStateAction<boolean>>;
  pinnedMenu: string[];
  sideBarToggle: boolean;
  setSideBarToggle: Dispatch<SetStateAction<boolean>>;
  searchableMenu: SearchableMenuType[];
  setSearchableMenu: Function;
  bookmarkList: SearchableMenuType[];
  setBookmarkList: Function;
};

export type SidebarChildrenType = {
  path?: string;
  title: string;
  nameArgument?: string;
  type: string;
  children?: SubChildrenType[];
  bookmark?: boolean;
  pathSlice?: string;
  claims?: string[];
};

export type SearchableMenuType = {
  icon: ReactNode;
  path: string;
  id: number;
  bookmarked?: boolean;
  title: string;
};

export type SubChildrenType = {
  title: string;
  type: string;
  path: string;
  bookmark?: boolean;
  claims?: string[];
};

export type SidebarItemType = {
  id?: number;
  title?: string | undefined;
  icon?: string | ReactNode | undefined;
  iconFill?: string | ReactNode;
  iconStroke?: string | ReactNode;
  nameArgument?: string;
  type: string;
  badge?: string;
  badge2?: boolean;
  badgeTxt?: string;
  pathSlice?: string;
  active?: boolean;
  path?: string;
  bookmark?: boolean;
  claims?: string[];
  children?: SidebarChildrenType[];
};

export type SidebarItemTypes = {
  item: SidebarItemType;
};

export type SidebarMenuType = {
  title?: string;
  menuContent?: string;
  claims?: string[];
  items: SidebarItemType[];
};

export type LanguageDataType = {
  name: string;
  shortName: string;
  iconClass: string;
  tag?: string;
};

export type ProfileType = {
  icon: ReactNode;
  path: string;
  text: string;
};

export type BookMarkContainPropsType = {
  handleSearch: (event: ChangeEvent<HTMLInputElement>) => void;
  searchedItems: SearchableMenuType[];
  handleBookmarkChange: (item: SearchableMenuType) => void;
  searchWord: string;
};

export type SearchBarContainPropsType = {
  handleSearch: (event: ChangeEvent<HTMLInputElement>) => void;
  suggestion: SearchableMenuType[];
  searchValue: string;
  setSearchValue: Function;
  fieldTouch: boolean;
  setFieldTouch: Function;
};

export type CheckLayoutDataType = {
  class?: string;
  image: string;
  title: string;
  slug: string;
  attr: string;
};
