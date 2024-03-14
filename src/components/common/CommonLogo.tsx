import Image from "next/image";
import React from "react";

type propsType = {
  alignLogo?: string;
};

export const CommonLogo = ({ alignLogo }: propsType) => {
  return (
    <a className={`logo ${alignLogo ? alignLogo : ""} `} href="/home">
      <Image
        width={100}
        height={37}
        className="img-fluid for-light"
        src={`/assets/images/logo/marqtize_logo.png`}
        alt="logo"
      />
      <Image
        width={100}
        height={37}
        className="img-fluid for-dark"
        src={`/assets/images/logo/marqtize_logo_dark.png`}
        alt="logo"
      />
    </a>
  );
};
