import React from "react";
import { Col } from "reactstrap";
import { Language } from "./Languages/index";
import { SearchBar } from "./SearchBar";
import { MoonLight } from "./MoonLight";
import Profile from "./Profile";

export const RightBar = () => {
  return (
    <Col
      xxl={7}
      xl={6}
      md={7}
      xs={8}
      className="nav-right pull-right right-header p-0 ms-auto"
    >
      <ul className="nav-menus flex-row">
        <Language />
        <SearchBar />
        <MoonLight />
        <Profile />
      </ul>
    </Col>
  );
};
