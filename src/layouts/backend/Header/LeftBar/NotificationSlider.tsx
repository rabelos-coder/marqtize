import dynamic from "next/dynamic";
import Image from "next/image";
import React from "react";

import { Link } from "@/navigation";

const Slider = dynamic(() => import("react-slick"), { ssr: false });

export const NotificationSlider = () => {
  // given slider options which i need ,you can add options which you want to use in slider
  const notificationSliderOption = {
    slidesToShow: 1,
    slidesToScroll: 1,
    dots: false,
    vertical: true,
    variableWidth: false,
    autoplay: true,
    autoplaySpeed: 2500,
    arrows: false,
  };

  return (
    <Slider
      className="notification-slider overflow-hidden m-0"
      {...notificationSliderOption}
    >
      <div className="d-flex h-100">
        <Image
          src={"/assets/images/giftools.gif"}
          alt="git"
          width={26}
          height={26}
        />
        <h6 className="mb-0 f-w-400">
          <span className="font-primary">Don&apos;t Miss Out!</span>
          <span className="f-light">Out new update has been release.</span>
        </h6>
        <i className="icon-arrow-top-right f-light" />
      </div>
      <div className="d-flex h-100">
        <Image
          src={"/assets/images/giftools.gif"}
          alt="git"
          width={26}
          height={26}
        />
        <h6 className="mb-0 f-w-400">
          <span className="f-light">Something you love is now on sale!</span>
        </h6>
        <Link
          className="ms-1"
          href="https://themeforest.net/user/pixelstrap/portfolio"
          target="_blank"
        >
          Buy now!
        </Link>
      </div>
    </Slider>
  );
};
