import React from "react";
import LayoutType from "./LayoutType";
import SidebarType from "./Sidebartype";
import SidebarIconType from "./SidebarIconType";
import ColorComponent from "./ColorComponent";
import MixLayoutComponent from "./MixLayoutComponent";

const SidebarCustomizer = () => {
  return (
    <>
      <LayoutType />
      <SidebarType />
      <SidebarIconType />
      <ColorComponent />
      <MixLayoutComponent />
    </>
  );
};

export default SidebarCustomizer;
