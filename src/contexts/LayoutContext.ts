import { LayoutContextType } from "@/types/layout";
import { createContext } from "react";

export const LayoutContext = createContext<LayoutContextType>({
  searchableMenu: [],
  setSearchableMenu: () => {},
  bookmarkList: [],
  setBookmarkList: () => {},
  searchIcon: false,
  sideBarToggle: false,
  setSideBarToggle: () => {},
  pinedMenu: [],
  setPinedMenu: () => {},
  setSearchIcon: () => {},
  bookMarkClass: false,
  setBookMarkClass: () => {},
});
