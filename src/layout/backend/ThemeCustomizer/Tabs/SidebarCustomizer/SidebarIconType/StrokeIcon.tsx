import { useTranslations } from "next-intl";
import React from "react";

import CommonUL from "../CommonUL";

type varType = {
  handleSideBarIconType: (data: string) => void;
  sideBarIconType: string;
};

const StrokeIcon = ({ handleSideBarIconType, sideBarIconType }: varType) => {
  const t = useTranslations("translations");

  return (
    <li
      data-attr="stroke-svg"
      className={`normal-sidebar border-0 ${sideBarIconType === "stroke-svg" ? "active" : ""}`}
      onClick={() => handleSideBarIconType("stroke-svg")}
    >
      <div className="header bg-light">
        <CommonUL />
      </div>
      <div className="body">
        <div className="body bg-light">
          <span className="badge badge-primary">{t("stroke")}</span>
        </div>
      </div>
    </li>
  );
};

export default StrokeIcon;
