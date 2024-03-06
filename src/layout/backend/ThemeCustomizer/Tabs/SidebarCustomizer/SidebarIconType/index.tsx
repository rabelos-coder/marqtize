import { useTranslations } from "next-intl";
import React from "react";

import Theme from "@/configs/theme";
import { useCustomizer } from "@/hooks";

import FillIcon from "./FillIcon";
import StrokeIcon from "./StrokeIcon";

const SidebarIconType = () => {
  const { addSidebarIconType } = useCustomizer();
  const sideBarIconType = Theme.data.settings.sidebar.iconType;

  const t = useTranslations("translations");

  const handleSideBarIconType = (type: string) => {
    addSidebarIconType(type);
  };

  return (
    <div>
      <h6>{t("sidebarIconType")}</h6>
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
