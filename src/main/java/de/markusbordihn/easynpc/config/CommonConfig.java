/**
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

package de.markusbordihn.easynpc.config;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod.EventBusSubscriber;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.loading.FMLPaths;
import net.minecraftforge.fml.loading.FileUtils;

import de.markusbordihn.easynpc.Constants;

@EventBusSubscriber(bus = EventBusSubscriber.Bus.MOD)
public class CommonConfig {
  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private CommonConfig() {}

  public static final ForgeConfigSpec commonSpec;
  public static final Config COMMON;

  static {
    com.electronwill.nightconfig.core.Config.setInsertionOrderPreserved(true);
    final Pair<Config, ForgeConfigSpec> specPair =
        new ForgeConfigSpec.Builder().configure(Config::new);
    commonSpec = specPair.getRight();
    COMMON = specPair.getLeft();
    log.info("Registering {} common config ...", Constants.MOD_NAME);
    try {
      FileUtils.getOrCreateDirectory(FMLPaths.CONFIGDIR.get(), Constants.MOD_ID);
    } catch (Exception exception) {
      log.error("There was an error, creating the directory:", exception);
    }
    ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, commonSpec,
        Constants.MOD_ID + "/" + Constants.MOD_ID + "-common.toml");
  }

  public static class Config {

    public final ForgeConfigSpec.BooleanValue mainConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue mainConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue mainConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue basicActionConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue basicActionConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue basicActionConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue dialogActionConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue dialogActionConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue dialogActionConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue basicDialogConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue basicDialogConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue basicDialogConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue yesNoDialogConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue yesNoDialogConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue yesNoDialogConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue noneDialogConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue noneDialogConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue noneDialogConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue defaultSkinConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultSkinConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultSkinConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue playerSkinConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue playerSkinConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue playerSkinConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue customSkinConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue customSkinConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue customSkinConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue defaultPoseConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultPoseConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultPoseConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue customPoseConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue customPoseConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue customPoseConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue equipmentConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue equipmentConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue equipmentConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue defaultPositionConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultPositionConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultPositionConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue defaultRotationConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue defaultRotationConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue defaultRotationConfigurationPermissionLevel;

    public final ForgeConfigSpec.BooleanValue scalingConfigurationEnabled;
    public final ForgeConfigSpec.BooleanValue scalingConfigurationAllowInCreative;
    public final ForgeConfigSpec.IntValue scalingConfigurationPermissionLevel;

    Config(ForgeConfigSpec.Builder builder) {
      builder.comment(Constants.MOD_NAME);

      builder.push("General");
      builder.pop();

      builder.push("Main Configuration");
      mainConfigurationEnabled =
          builder.comment("Enable main configuration.").define("mainConfigurationEnabled", true);
      mainConfigurationAllowInCreative =
          builder.comment("Allow main configuration in creative mode.")
              .define("mainConfigurationAllowInCreative", true);
      mainConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Main Configuration"))
              .defineInRange("mainConfigurationPermissionLevel", 0, 0, 4);

      builder.push("Basic Action Configuration");
      basicActionConfigurationEnabled = builder.comment("Enable basic action configuration.")
          .define("basicActionConfigurationEnabled", true);
      basicActionConfigurationAllowInCreative =
          builder.comment("Allow basic action configuration in creative mode.")
              .define("basicActionConfigurationAllowInCreative", true);
      basicActionConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Basic Action Configuration"))
              .defineInRange("basicActionConfigurationPermissionLevel", 1, 0, 4);
      builder.pop();

      builder.push("Dialog Action Configuration");
      dialogActionConfigurationEnabled = builder.comment("Enable dialog action configuration.")
          .define("dialogActionConfigurationEnabled", true);
      dialogActionConfigurationAllowInCreative =
          builder.comment("Allow dialog action configuration in creative mode.")
              .define("dialogActionConfigurationAllowInCreative", true);
      dialogActionConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Dialog Action Configuration"))
              .defineInRange("dialogActionConfigurationPermissionLevel", 1, 0, 4);
      builder.pop();

      builder.push("Basic Dialog Configuration");
      basicDialogConfigurationEnabled = builder.comment("Enable basic dialog configuration.")
          .define("basicDialogConfigurationEnabled", true);
      basicDialogConfigurationAllowInCreative =
          builder.comment("Allow basic dialog configuration in creative mode.")
              .define("basicDialogConfigurationAllowInCreative", true);
      basicDialogConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Basic Dialog Configuration"))
              .defineInRange("basicDialogConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("Yes/No Dialog Configuration");
      yesNoDialogConfigurationEnabled = builder.comment("Enable yes/no dialog configuration.")
          .define("yesNoDialogConfigurationEnabled", true);
      yesNoDialogConfigurationAllowInCreative =
          builder.comment("Allow yes/no dialog configuration in creative mode.")
              .define("yesNoDialogConfigurationAllowInCreative", true);
      yesNoDialogConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Yes/No Dialog Configuration"))
              .defineInRange("yesNoDialogConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("None Dialog Configuration");
      noneDialogConfigurationEnabled = builder.comment("Enable none dialog configuration.")
          .define("noneDialogConfigurationEnabled", true);
      noneDialogConfigurationAllowInCreative =
          builder.comment("Allow none dialog configuration in creative mode.")
              .define("noneDialogConfigurationAllowInCreative", true);
      noneDialogConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("None Dialog Configuration"))
              .defineInRange("noneDialogConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("Default Skin Configuration");
      defaultSkinConfigurationEnabled = builder.comment("Enable default skin configuration.")
          .define("defaultSkinConfigurationEnabled", true);
      defaultSkinConfigurationAllowInCreative =
          builder.comment("Allow default skin configuration in creative mode.")
              .define("defaultSkinConfigurationAllowInCreative", true);
      defaultSkinConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Default Skin Configuration"))
              .defineInRange("defaultSkinConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("Player Skin Configuration");
      playerSkinConfigurationEnabled = builder.comment("Enable player skin configuration.")
          .define("playerSkinConfigurationEnabled", true);
      playerSkinConfigurationAllowInCreative =
          builder.comment("Allow player skin configuration in creative mode.")
              .define("playerSkinConfigurationAllowInCreative", true);
      playerSkinConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Player Skin Configuration"))
              .defineInRange("playerSkinConfigurationPermissionLevel", 1, 0, 4);
      builder.pop();

      builder.push("Custom Skin Configuration");
      customSkinConfigurationEnabled = builder.comment("Enable custom skin configuration.")
          .define("customSkinConfigurationEnabled", true);
      customSkinConfigurationAllowInCreative =
          builder.comment("Allow custom skin configuration in creative mode.")
              .define("customSkinConfigurationAllowInCreative", true);
      customSkinConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Custom Skin Configuration"))
              .defineInRange("customSkinConfigurationPermissionLevel", 2, 0, 4);
      builder.pop();

      builder.push("Default Pose Configuration");
      defaultPoseConfigurationEnabled = builder.comment("Enable default pose configuration.")
          .define("defaultPoseConfigurationEnabled", true);
      defaultPoseConfigurationAllowInCreative =
          builder.comment("Allow default pose configuration in creative mode.")
              .define("defaultPoseConfigurationAllowInCreative", true);
      defaultPoseConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Default Pose Configuration"))
              .defineInRange("defaultPoseConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("Custom Pose Configuration");
      customPoseConfigurationEnabled = builder.comment("Enable custom pose configuration.")
          .define("customPoseConfigurationEnabled", true);
      customPoseConfigurationAllowInCreative =
          builder.comment("Allow custom pose configuration in creative mode.")
              .define("customPoseConfigurationAllowInCreative", true);
      customPoseConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Custom Pose Configuration"))
              .defineInRange("customPoseConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("Equipment Configuration");
      equipmentConfigurationEnabled = builder.comment("Enable equipment configuration.")
          .define("equipmentConfigurationEnabled", true);
      equipmentConfigurationAllowInCreative =
          builder.comment("Allow equipment configuration in creative mode.")
              .define("equipmentConfigurationAllowInCreative", true);
      equipmentConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Equipment Configuration"))
              .defineInRange("equipmentConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("Position Configuration");
      defaultPositionConfigurationEnabled = builder.comment("Enable position configuration.")
          .define("defaultPositionConfigurationEnabled", true);
      defaultPositionConfigurationAllowInCreative =
          builder.comment("Allow position configuration in creative mode.")
              .define("defaultPositionConfigurationAllowInCreative", true);
      defaultPositionConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Position Configuration"))
              .defineInRange("defaultPositionConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("Rotation Configuration");
      defaultRotationConfigurationEnabled = builder.comment("Enable rotation configuration.")
          .define("defaultRotationConfigurationEnabled", true);
      defaultRotationConfigurationAllowInCreative =
          builder.comment("Allow rotation configuration in creative mode.")
              .define("defaultRotationConfigurationAllowInCreative", true);
      defaultRotationConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Rotation Configuration"))
              .defineInRange("defaultRotationConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();

      builder.push("Scale Configuration");
      scalingConfigurationEnabled =
          builder.comment("Enable scale configuration.").define("scaleConfigurationEnabled", true);
      scalingConfigurationAllowInCreative =
          builder.comment("Allow scale configuration in creative mode.")
              .define("scaleConfigurationAllowInCreative", true);
      scalingConfigurationPermissionLevel =
          builder.comment(getPermissionLevelComment("Scale Configuration"))
              .defineInRange("scaleConfigurationPermissionLevel", 0, 0, 4);
      builder.pop();
    }
  }

  public static String getPermissionLevelComment(String name) {
    return name
        + " permission level. (0 = everyone, 1 = moderator, 2 = gamemaster, 3 = admin, 4 = owner)";
  }
}
