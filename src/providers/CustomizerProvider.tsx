"use client";

import React, { useEffect, useState } from "react";

import { classes } from "@/configs/layout";
import Theme from "@/configs/theme";
import { CustomizerContext } from "@/context/CustomizerContext";
import { ChildrenProps } from "@/types/common";

export const CustomizerProvider = ({ children }: ChildrenProps) => {
  const [layout, setLayout] = useState(Theme.data.settings.layout_class);
  const [layoutName, setLayoutName] = useState("");
  const [sidebarIconType, setSidebarIconType] = useState(
    Theme.data.settings.sidebar.iconType
  );
  const [mix_background_layout, setMixBackgroundLayout] = useState(
    Theme.data.color.mix_background_layout
  );
  const [toggleIcon, setToggleIcon] = useState(false);
  const [mixLayout, setMixLayout] = useState(false);
  const [sidebarResponsive, setSidebarResponsive] = useState(false);
  const [IsOpen, setIsClose] = useState(false);

  useEffect(() => {
    classes.map((item) => {
      if (item.name === layoutName) {
        Theme.data.settings.layout_class = item.class;
        setLayout(item.class);
      }
    });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [layoutName]);

  //Set LTR,RTL,BOX Tyoe
  const addLayout = (layout: string) => {
    Theme.data.settings.layout_type = layout;
    setLayout(layout);
  };

  //Toggle sidebar
  const toggleSidebar = (toggle: boolean) => {
    setToggleIcon(toggle);
  };

  //Multiple Sidebar Layouts
  const addSidebarLayouts = (sidebar_layout: string) => {
    Theme.data.settings.layout_class = sidebar_layout;
    setLayout(sidebar_layout);
  };

  //SideBar Icon Sidebar
  const addSidebarIconType = (sidebar_Icon_Type: string) => {
    Theme.data.settings.sidebar.iconType = sidebar_Icon_Type;
    setSidebarIconType(sidebar_Icon_Type);
  };

  //Add Mix layouts like (dark , light ,...)
  const addMixBackgroundLayout = (mix_background_layout: string) => {
    Theme.data.color.mix_background_layout = mix_background_layout;
    if (mix_background_layout !== "light-only") {
      setMixLayout(false);
    } else {
      setMixLayout(true);
    }
    setMixBackgroundLayout(mix_background_layout);
  };

  // Add Colors
  const addColor = (default_color: string, secondary_color: string) => {
    Theme.data.color.primary_color = default_color;
    Theme.data.color.secondary_color = secondary_color;
  };

  const toggleSidebarResponsive = (toggle: boolean) => {
    setSidebarResponsive(toggle);
  };

  return (
    <CustomizerContext.Provider
      value={{
        layout,
        setLayout,
        IsOpen,
        mixLayout,
        layoutName,
        toggleIcon,
        setToggleIcon,
        mix_background_layout,
        addLayout,
        toggleSidebar,
        setLayoutName,
        sidebarResponsive,
        sidebarIconType,
        setMixLayout,
        setIsClose,
        addSidebarLayouts,
        setSidebarResponsive,
        addSidebarIconType,
        addMixBackgroundLayout,
        toggleSidebarResponsive,
        addColor,
      }}
    >
      {children}
    </CustomizerContext.Provider>
  );
};
