import { CustomizerContextType } from "@/types/customizer";
import { createContext } from "react";

export const CustomizerContext = createContext<CustomizerContextType>({
  layout: "",
  sidebarIconType: "",
  layoutName: "",
  mixLayout: false,
  toggleIcon: false,
  mix_background_layout: "",
  sidebarResponsive: false,
  IsOpen: false,
  setIsClose: () => {},
  setLayoutName: () => {},
  toggleSidebar: () => {},
  addLayout: () => {},
  setLayout: () => {},
  setMixLayout: () => {},
  toggleSidebarResponsive: () => {},
  setToggleIcon: () => {},
  addSidebarLayouts: () => {},
  addSidebarIconType: () => {},
  setSidebarResponsive: () => {},
  addColor: () => {},
  addMixBackgroundLayout: () => {},
});
