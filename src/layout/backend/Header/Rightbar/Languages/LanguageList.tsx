import { useTranslations } from "next-intl";
import React, { Dispatch, SetStateAction } from "react";

import { useAppDispatch } from "@/hooks";
import { usePathname, useRouter } from "@/navigation";
import { setLanguage } from "@/store/slices/authSlice";
import { LanguagesData } from "@/types/language";

type ListStateType = {
  selected: string;
  setSelected: Dispatch<SetStateAction<string>>;
};

export const LanguageList = ({ selected, setSelected }: ListStateType) => {
  const pathname = usePathname();
  const router = useRouter();

  const t = useTranslations();
  const dispatch = useAppDispatch();

  const changeLanguage = (locale: string) => {
    setSelected(locale);
    dispatch(setLanguage(locale));
    router.push(pathname, { locale });
  };

  return (
    <div className={`more_lang ${selected ? "active" : ""}`}>
      {LanguagesData &&
        LanguagesData.map((item, index) => (
          <div
            key={index}
            className="lang"
            onClick={() => changeLanguage(item.shortName)}
          >
            <i className={item.iconClass}></i>
            <span className="lang-txt">
              {t(item.name)}
              {item.tag && <span> {item.tag}</span>}
            </span>
          </div>
        ))}
    </div>
  );
};
