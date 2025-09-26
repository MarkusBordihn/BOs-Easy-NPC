/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.data.configuration;

import de.markusbordihn.easynpc.data.model.ModelPose;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.TradingDataCapable;

public class ConfigurationTypeHelper {

  private ConfigurationTypeHelper() {}

  public static ConfigurationType resolveConfigurationTypeAlias(
      final ConfigurationType configurationType, final EasyNPC<?> easyNPC) {
    if (!configurationType.isAlias()) {
      return configurationType;
    }

    if (configurationType == ConfigurationType.DIALOG) {
      DialogDataCapable<?> dialogData = easyNPC.getEasyNPCDialogData();
      if (dialogData != null && dialogData.getDialogDataSet() != null) {
        return switch (dialogData.getDialogDataSet().getType()) {
          case NONE -> ConfigurationType.NONE_DIALOG;
          case YES_NO -> ConfigurationType.YES_NO_DIALOG;
          case CUSTOM, STANDARD -> ConfigurationType.ADVANCED_DIALOG;
          default -> ConfigurationType.BASIC_DIALOG;
        };
      }
    } else if (configurationType == ConfigurationType.SKIN) {
      SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
      return switch (skinData.getSkinType()) {
        case NONE -> ConfigurationType.NONE_SKIN;
        case PLAYER_SKIN -> ConfigurationType.PLAYER_SKIN;
        case SECURE_REMOTE_URL, INSECURE_REMOTE_URL -> ConfigurationType.URL_SKIN;
        case CUSTOM -> ConfigurationType.CUSTOM_SKIN;
        default -> ConfigurationType.DEFAULT_SKIN;
      };
    } else if (configurationType == ConfigurationType.TRADING) {
      TradingDataCapable<?> tradingData = easyNPC.getEasyNPCTradingData();
      return switch (tradingData.getTradingDataSet().getType()) {
        case ADVANCED -> ConfigurationType.ADVANCED_TRADING;
        case BASIC -> ConfigurationType.BASIC_TRADING;
        case CUSTOM -> ConfigurationType.CUSTOM_TRADING;
        default -> ConfigurationType.NONE_TRADING;
      };
    } else if (configurationType == ConfigurationType.POSE) {
      ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
      if (modelData.getModelPose() == ModelPose.CUSTOM) {
        if (modelData.hasChangedModelScale()) {
          return ConfigurationType.CUSTOM_POSE;
        } else if (modelData.hasChangedModelRotation()) {
          return ConfigurationType.ADVANCED_POSE;
        }
        return ConfigurationType.BASIC_POSE;
      }
      return ConfigurationType.DEFAULT_POSE;
    }

    return configurationType;
  }
}
