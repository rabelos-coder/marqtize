import React from "react";
import { useCustomizer } from "@/hooks";
import { CheckLayoutData } from "@/configs/customizer";

const CheckLayout = () => {
  const { setLayoutName } = useCustomizer();

  const handlePageLayputs = (type: string) => {
    setLayoutName(type.toLowerCase().replace(" ", ""));
  };

  return (
    <ul className="sidebar-type layout-grid layout-types">
      {CheckLayoutData &&
        CheckLayoutData.map((item, index) => (
          <li
            key={index}
            data-attr={item.attr}
            className={`${item.class ? item.class : ""}`}
            onClick={() => {
              handlePageLayputs(item.title);
            }}
          >
            <div className="layout-img">
              {/*  eslint-disable-next-line @next/next/no-img-element */}
              <img
                src={`/assets/images/${item.image}`}
                className="img-fluid"
                alt="layout Type"
              />
              <h6>{item.title}</h6>
            </div>
          </li>
        ))}
    </ul>
  );
};

export default CheckLayout;
