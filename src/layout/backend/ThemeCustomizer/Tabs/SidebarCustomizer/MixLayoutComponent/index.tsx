import { useTranslations } from "next-intl";
import React, { useEffect } from "react";

import Theme from "@/configs/theme";
import { useCustomizer } from "@/hooks";

import BgDark from "./BgDark";
import BgLight from "./BgLight";
import DarkSidebar from "./DarkSidebar";

const MixLayoutComponent = () => {
  const { addMixBackgroundLayout, setMixLayout } = useCustomizer();
  const mixLayout = Theme.data.color.mix_background_layout;

  const t = useTranslations("translations");

  useEffect(() => {
    if (mixLayout !== "light-only") {
      setMixLayout(false);
    } else {
      setMixLayout(true);
    }
    Theme.data.color.mix_background_layout = mixLayout;
    if (typeof document !== "undefined") document.body.classList.add(mixLayout);
  }, [mixLayout, setMixLayout]);

  const handleCustomizerMix_Background = (value: string) => {
    addMixBackgroundLayout(value);
    if (typeof document !== "undefined") {
      if (value === "light-only") {
        document.body.classList.add("light-only");
        document.body.classList.remove("dark-sidebar");
        document.body.classList.remove("dark-only");
      } else if (value === "dark-sidebar") {
        document.body.classList.remove("light-only");
        document.body.classList.add("dark-sidebar");
        document.body.classList.remove("dark-only");
      } else if (value === "dark-only") {
        document.body.classList.remove("light-only");
        document.body.classList.remove("dark-sidebar");
        document.body.classList.add("dark-only");
      }
    }
  };

  return (
    <>
      <h6>{t("mixLayout")}</h6>
      <ul className="layout-grid customizer-mix flex-row">
        <BgLight
          mixLayout={mixLayout}
          handleCustomizerMix_Background={handleCustomizerMix_Background}
        />
        <DarkSidebar
          mixLayout={mixLayout}
          handleCustomizerMix_Background={handleCustomizerMix_Background}
        />
        <BgDark
          handleCustomizerMix_Background={handleCustomizerMix_Background}
          mixLayout={mixLayout}
        />
      </ul>
    </>
  );
};

export default MixLayoutComponent;
