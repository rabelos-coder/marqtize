"use client";

import { LayoutContext } from "@/contexts/LayoutContext";
import { ChildrenProps } from "@/types/children";
import { SearchableMenuType } from "@/types/layout";
import React, { useState } from "react";

export const LayoutProvider = ({ children }: ChildrenProps) => {
  const [searchIcon, setSearchIcon] = useState(false);
  const [bookMarkClass, setBookMarkClass] = useState(false);
  const [pinedMenu, setPinedMenu] = useState<string[]>([""]);
  const [sideBarToggle, setSideBarToggle] = useState(false);
  const [searchableMenu, setSearchableMenu] = useState([]);
  const [bookmarkList, setBookmarkList] = useState<SearchableMenuType[]>([]);

  return (
    <LayoutContext.Provider
      value={{
        bookmarkList,
        setBookmarkList,
        searchIcon,
        setSearchIcon,
        bookMarkClass,
        setBookMarkClass,
        pinedMenu,
        setPinedMenu,
        sideBarToggle,
        setSideBarToggle,
        searchableMenu,
        setSearchableMenu,
      }}
    >
      {children}
    </LayoutContext.Provider>
  );
};
