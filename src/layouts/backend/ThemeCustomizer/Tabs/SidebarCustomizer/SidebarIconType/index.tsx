import React from "react";
import StrokeIcon from "./StrokeIcon";
import FillIcon from "./FillIcon";
import { useCustomizer } from "@/hooks";
import Theme from "@/configs/theme";

const SidebarIconType = () => {
  const { addSidebarIconType } = useCustomizer();
  const sideBarIconType = Theme.data.settings.sidebar.iconType;

  const handleSideBarIconType = (type: string) => {
    addSidebarIconType(type);
  };

  return (
    <div>
      <h6>Sidebar Icon</h6>
      <ul className="sidebar-type layout-grid flex-row">
        <StrokeIcon
          handleSideBarIconType={handleSideBarIconType}
          sideBarIconType={sideBarIconType}
        />
        <FillIcon
          handleSideBarIconType={handleSideBarIconType}
          sideBarIconType={sideBarIconType}
        />
      </ul>
    </div>
  );
};

export default SidebarIconType;
