import React from "react";
import { Nav, NavItem, NavLink } from "reactstrap";

type NavCustomizerType = {
  callbackNav: (test: string, open: boolean) => void;
  selected: string;
};
const NavCustomizer = ({ callbackNav, selected }: NavCustomizerType) => {
  return (
    <Nav className="flex-column nac-pills">
      <NavItem>
        <NavLink
          className={selected === "check-layout" ? "active" : ""}
          onClick={() => callbackNav("check-layout", true)}
        >
          <div className="settings">
            <i className="icon-paint-bucket"></i>
          </div>
          <span>Check layouts</span>
        </NavLink>
      </NavItem>
      <NavItem>
        <NavLink
          className={selected === "sidebar-type" ? "active" : ""}
          onClick={() => callbackNav("sidebar-type", true)}
        >
          <div className="settings">
            <i className="icon-settings"></i>
          </div>
          <span>Quick option</span>
        </NavLink>
      </NavItem>
    </Nav>
  );
};

export default NavCustomizer;
