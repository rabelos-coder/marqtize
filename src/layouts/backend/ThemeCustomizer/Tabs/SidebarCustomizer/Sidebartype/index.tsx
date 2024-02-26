import React, { useContext } from "react";
import Vertical from "./Vertical";
import Horizontal from "./Horizontal";
import { useCustomizer } from "@/hooks";

const SidebarType = () => {
  const { addSidebarLayouts, layout } = useCustomizer();

  const handleSidebarType = (type: string) => {
    addSidebarLayouts(type);
  };
  return (
    <div>
      <h6>Sidebar Icon</h6>
      <ul className="sidebar-type layout-grid">
        <Vertical handleSidebarType={handleSidebarType} layout={layout} />
        <Horizontal handleSidebarType={handleSidebarType} layout={layout} />
      </ul>
    </div>
  );
};

export default SidebarType;
