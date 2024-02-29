import { createContext } from "react";

import { LayoutContextType } from "@/types/layout";

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
