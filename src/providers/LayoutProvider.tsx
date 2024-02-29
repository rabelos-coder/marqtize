"use client";

import React, { useState } from "react";

import { LayoutContext } from "@/context/LayoutContext";
import { useAppSelector } from "@/hooks";
import { ChildrenProps } from "@/types/common";
import { SearchableMenuType } from "@/types/layout";

export const LayoutProvider = ({ children }: ChildrenProps) => {
  const [searchIcon, setSearchIcon] = useState(false);
  const [bookMarkClass, setBookMarkClass] = useState(false);
  const [sideBarToggle, setSideBarToggle] = useState(false);
  const [searchableMenu, setSearchableMenu] = useState([]);
  const [bookmarkList, setBookmarkList] = useState<SearchableMenuType[]>([]);
  const { pinnedMenu } = useAppSelector((state) => state.theme);

  return (
    <LayoutContext.Provider
      value={{
        bookmarkList,
        setBookmarkList,
        searchIcon,
        setSearchIcon,
        bookMarkClass,
        setBookMarkClass,
        pinnedMenu,
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
