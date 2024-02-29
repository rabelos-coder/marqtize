import { ChangeEvent, Dispatch, ReactNode, SetStateAction } from "react";
import * as Icon from "react-feather";

export type LayoutContextType = {
  searchIcon: boolean;
  bookMarkClass: boolean;
  setBookMarkClass: Dispatch<SetStateAction<boolean>>;
  setSearchIcon: Dispatch<SetStateAction<boolean>>;
  setPinedMenu: Dispatch<SetStateAction<string[]>>;
  pinedMenu: string[];
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
  type: string;
  children?: SubChildrenType[];
  bookmark?: boolean;
  pathSlice?: string;
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
};

export type SidebarItemType = {
  id?: number;
  title?: string | undefined;
  icon?: string;
  type: string;
  badge?: string;
  badge2?: boolean;
  badgeTxt?: string;
  pathSlice?: string;
  active?: boolean;
  path?: string;
  bookmark?: boolean;
  children?: SidebarChildrenType[];
};

export type SidebarItemTypes = {
  item: SidebarItemType;
};

export type SidebarMenuType = {
  title?: string;
  menuContent?: string;
  items: SidebarItemType[];
};

export type LanguageDataType = {
  name: string;
  shortName: string;
  iconClass: string;
  tag?: string;
};

export type ProfileType = {
  icon: keyof typeof Icon;
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
