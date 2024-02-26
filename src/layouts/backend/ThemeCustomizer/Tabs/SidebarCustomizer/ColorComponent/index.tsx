import React, { useEffect, useState } from "react";
import { Button, Input } from "reactstrap";

import Theme from "@/configs/theme";
import { useCustomizer } from "@/hooks";

const ColorComponent = () => {
  const { addColor } = useCustomizer();
  const default_color = Theme.data.color.primary_color;
  const secondary_color = Theme.data.color.secondary_color;
  const [colorBackground1, setColorBackground1] = useState(default_color);
  const [colorBackground2, setColorBackground2] = useState(secondary_color);

  useEffect(() => {
    if (typeof document !== "undefined") {
      document.documentElement.style.setProperty(
        "--theme-deafult",
        colorBackground1
      );
      document.documentElement.style.setProperty(
        "--theme-secondary",
        colorBackground2
      );
    }
    Theme.data.color.primary_color = colorBackground1;
    Theme.data.color.secondary_color = colorBackground2;
  }, [
    setColorBackground1,
    setColorBackground2,
    colorBackground1,
    colorBackground2,
  ]);

  const handleUnlimatedColor1Change = (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    const { value } = e.target;
    setColorBackground1(value);
    Theme.data.color.primary_color = value;
  };
  const handleUnlimatedColor2Change = (
    e: React.ChangeEvent<HTMLInputElement>
  ) => {
    const { value } = e.target;
    setColorBackground2(value);
    Theme.data.color.secondary_color = value;
  };
  const OnUnlimatedColorClick = () => {
    Theme.data.color.primary_color = colorBackground1;
    Theme.data.color.secondary_color = colorBackground2;
    addColor(colorBackground1, colorBackground2);
    if (typeof document !== "undefined") {
      document.documentElement.style.setProperty(
        "--theme-deafult",
        colorBackground1
      );
      document.documentElement.style.setProperty(
        "--theme-secondary",
        colorBackground2
      );
    }
  };

  return (
    <>
      <h6>Unlimited Color</h6>
      <ul className="simple-list flex-row layout-grid unlimited-color-layout">
        <Input
          className="p-0"
          type="color"
          name="Color-Background1"
          value={colorBackground1}
          onChange={(e) => handleUnlimatedColor1Change(e)}
        />
        <Input
          className="p-0"
          type="color"
          name="Color-Background2"
          value={colorBackground2}
          onChange={(e) => handleUnlimatedColor2Change(e)}
        />
        <Button
          color="primary"
          className="color-apply-btn color-apply-btn"
          onClick={OnUnlimatedColorClick}
        >
          Apply
        </Button>
      </ul>
    </>
  );
};

export default ColorComponent;
