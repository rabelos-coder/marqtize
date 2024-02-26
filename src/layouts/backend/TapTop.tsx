import React, { useEffect, useState } from "react";
import { ChevronsUp } from "react-feather";

export const TapTop = () => {
  const [tapTopStyle, setTapTopStyle] = useState("none");

  const executeScroll = () => {
    if (typeof window !== "undefined")
      window.scrollTo({ top: 0, left: 0, behavior: "smooth" });
  };

  const handleScroll = () => {
    if (typeof window !== "undefined" && window.scrollY > 100) {
      setTapTopStyle("block");
    } else {
      setTapTopStyle("none");
    }
  };

  useEffect(() => {
    if (typeof window !== "undefined")
      window.addEventListener("scroll", handleScroll);
    handleScroll();

    return () => {
      if (typeof window !== "undefined")
        window.removeEventListener("scroll", handleScroll);
    };
  }, []);

  return (
    <div className="tap-top" style={{ display: tapTopStyle }}>
      <ChevronsUp onClick={() => executeScroll()} />
    </div>
  );
};
