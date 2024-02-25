import { usePathname, useRouter } from "@/navigation";
import { Languages } from "@/types/language";
import React, { Dispatch, SetStateAction } from "react";

type ListStateType = {
  selected: string;
  setSelected: Dispatch<SetStateAction<string>>;
};

export const LanguageList = ({ selected, setSelected }: ListStateType) => {
  const pathname = usePathname();
  const router = useRouter();

  const changeLanguage = (locale: string) => {
    setSelected(locale);
    router.push(pathname, { locale });
  };

  return (
    <div className={`more_lang ${selected ? "active" : ""}`}>
      {Languages &&
        Languages.map((item, index) => (
          <div
            key={index}
            className="lang"
            onClick={() => changeLanguage(item.shortName)}
          >
            <i className={item.iconClass}></i>
            <span className="lang-txt">
              {item.name}
              {item.tag && <span> {item.tag}</span>}
            </span>
          </div>
        ))}
    </div>
  );
};
