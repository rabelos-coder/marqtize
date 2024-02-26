import { useEffect, useState } from "react";

import SvgIcon from "@/components/common/Icons/SvgIcon";
import { useAppDispatch, useAppSelector } from "@/hooks";
import { setTheme } from "@/store/slices/themeSlice";

export const MoonLight = () => {
  const [darkMode, setDarkMode] = useState(false);

  const dispatch = useAppDispatch();
  const { theme } = useAppSelector((state) => state.theme);

  const DarkModeHandler = (dark: boolean) => {
    if (dark) {
      if (typeof document !== "undefined") {
        document.body.classList.remove("dark-only");
        document.body.classList.add("light-only");
      }
      setDarkMode(!darkMode);
      dispatch(setTheme("light"));
    } else {
      if (typeof document !== "undefined") {
        document.body.classList.remove("light-only");
        document.body.classList.add("dark-only");
      }
      setDarkMode(!darkMode);
      dispatch(setTheme("dark"));
    }
  };

  useEffect(() => {
    if (theme === "light") {
      if (typeof document !== "undefined") {
        document.body.classList.remove("dark-only");
        document.body.classList.add("light-only");
      }
      setDarkMode(false);
    } else {
      if (typeof document !== "undefined") {
        document.body.classList.remove("light-only");
        document.body.classList.add("dark-only");
      }
      setDarkMode(true);
    }
  }, [theme]);

  return (
    <li>
      <div
        className={`mode ${darkMode ? "active" : ""}`}
        onClick={() => DarkModeHandler(darkMode)}
      >
        <SvgIcon iconId={darkMode ? "sun" : "moon"} />
      </div>
    </li>
  );
};
