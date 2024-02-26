import React, { useState } from "react";
import LTR from "./LTR";
import RTL from "./RTL";
import BoxLayout from "./BoxLayout";
import Theme from "@/configs/theme";

const LayoutType = () => {
  const localStorageLayout = Theme.data.settings.layout_type;
  const [layout_type, setLayout_type] = useState(localStorageLayout);

  const handleLayout = (layout: string) => {
    setLayout_type(layout);
    if (typeof document !== "undefined") {
      if (layout === "rtl") {
        document.body.classList.add("rtl");
        document.body.classList.remove("ltr");
        document.body.classList.remove("box-layout");
        document.documentElement.dir = "rtl";
      } else if (layout === "ltr") {
        document.body.classList.add("ltr");
        document.body.classList.remove("rtl");
        document.body.classList.remove("box-layout");
        document.documentElement.dir = "ltr";
      } else if (layout === "box-layout") {
        document.body.classList.remove("ltr");
        document.body.classList.remove("rtl");
        document.body.classList.add("box-layout");
        document.body.classList.remove("offcanvas");
        document.documentElement.dir = "ltr";
      }
    }
  };

  return (
    <div>
      <h6 className="mt-0 pt-0">Layout Type</h6>
      <ul className="main-layout layout-grid flex-row">
        <LTR handleLayout={handleLayout} layout_type={layout_type} />
        <RTL handleLayout={handleLayout} layout_type={layout_type} />
        <BoxLayout handleLayout={handleLayout} layout_type={layout_type} />
      </ul>
    </div>
  );
};

export default LayoutType;
