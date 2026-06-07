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

package de.markusbordihn.easynpc.server.commands;

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.display.NameVisibilityType;
import de.markusbordihn.easynpc.data.model.RootModelData;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.debug.DebugManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.VariantDataCapable;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.network.chat.Component;
import net.minecraft.server.permissions.Permissions;
import net.minecraft.world.entity.EntityDimensions;

public class DebugCommand extends Command {

  private DebugCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("debug")
        .requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER))
        .then(
            Commands.literal("npc")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                showNPCDebug(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)))))
        .then(
            Commands.literal("core")
                .then(
                    Commands.argument(ENABLE_ARG, BoolArgumentType.bool())
                        .executes(
                            context ->
                                setDebug(
                                    context.getSource(),
                                    BoolArgumentType.getBool(context, ENABLE_ARG)))));
  }

  public static int setDebug(CommandSourceStack context, boolean enable) {
    if (enable) {
      sendSuccessMessage(
          context,
          "► Enable debug for "
              + Constants.MOD_NAME
              + ", please check debug.log for the full output.",
          ChatFormatting.GREEN);
      sendSuccessMessage(
          context,
          "> Use '/" + Constants.MOD_COMMAND + " debug false' to disable the debug!",
          ChatFormatting.WHITE);
    } else {
      sendSuccessMessage(
          context, "■ Disable debug for " + Constants.MOD_NAME + "!", ChatFormatting.RED);
      sendSuccessMessage(
          context,
          "> Please check the latest.log and/or debug.log for the full output.",
          ChatFormatting.WHITE);
    }
    DebugManager.enableDebugLevel(enable);

    return Command.SINGLE_SUCCESS;
  }

  private static int showNPCDebug(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return sendFailureMessage(context, "Invalid EasyNPC target");
    }

    Component customName = easyNPC.getEntity().getCustomName();
    String customNameText = customName != null ? customName.getString() : "<none>";
    DisplayAttributeDataCapable<?> displayData = easyNPC.getEasyNPCDisplayAttributeData();
    NameVisibilityType nameVisibilityType =
        displayData != null
            ? displayData.getDisplayEnumAttribute(
                DisplayAttributeType.NAME_VISIBILITY, NameVisibilityType.class)
            : null;
    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    SkinDataEntry skinDataEntry = skinData != null ? skinData.getSkinDataEntry() : null;
    VariantDataCapable<?> variantData = easyNPC.getEasyNPCVariantData();
    String variantName =
        variantData != null && variantData.getSkinVariantType() != null
            ? variantData.getSkinVariantType().name()
            : "<missing>";
    ModelDataCapable<?> modelData = easyNPC.getEasyNPCModelData();
    RootModelData rootModelData =
        modelData != null ? modelData.getModelRootData() : RootModelData.DEFAULT;
    EntityDimensions entityDimensions =
        easyNPC.getEntity().getDimensions(easyNPC.getEntity().getPose());

    sendSuccessMessage(context, "Easy NPC Debug: " + easyNPC.getEntityUUID(), ChatFormatting.GOLD);
    sendSuccessMessage(
        context,
        "Type: " + easyNPC.getEntity().getType() + " / Pose: " + easyNPC.getEntity().getPose(),
        ChatFormatting.WHITE);
    sendSuccessMessage(
        context,
        "Name: " + customNameText + " / Visible: " + easyNPC.getEntity().isCustomNameVisible(),
        ChatFormatting.WHITE);
    sendSuccessMessage(
        context,
        "Name Visibility Attribute: "
            + (nameVisibilityType != null ? nameVisibilityType : "<missing>"),
        ChatFormatting.WHITE);
    sendSuccessMessage(context, "Skin: " + formatSkinData(skinDataEntry), ChatFormatting.WHITE);
    sendSuccessMessage(context, "Variant: " + variantName, ChatFormatting.WHITE);
    sendSuccessMessage(
        context,
        "Root Scale: " + rootModelData.scale() + " / Root Rotation: " + rootModelData.rotation(),
        ChatFormatting.WHITE);
    sendSuccessMessage(
        context,
        "Dimensions: "
            + entityDimensions.width()
            + " x "
            + entityDimensions.height()
            + " / BB Height: "
            + easyNPC.getEntity().getBbHeight(),
        ChatFormatting.WHITE);

    log.info(
        "[Debug NPC] uuid={}, type={}, customName={}, visible={}, nameVisibility={}, skin={}, variant={}, rootScale={}, rootRotation={}, dimensions={}x{}, bbHeight={}",
        easyNPC.getEntityUUID(),
        easyNPC.getEntity().getType(),
        customNameText,
        easyNPC.getEntity().isCustomNameVisible(),
        nameVisibilityType,
        skinDataEntry,
        variantName,
        rootModelData.scale(),
        rootModelData.rotation(),
        entityDimensions.width(),
        entityDimensions.height(),
        easyNPC.getEntity().getBbHeight());

    return Command.SINGLE_SUCCESS;
  }

  private static String formatSkinData(SkinDataEntry skinDataEntry) {
    if (skinDataEntry == null) {
      return "<missing>";
    }

    return "type="
        + skinDataEntry.type()
        + ", name="
        + skinDataEntry.name()
        + ", url="
        + skinDataEntry.url()
        + ", uuid="
        + skinDataEntry.uuid()
        + ", disableLayers="
        + skinDataEntry.disableLayers()
        + ", contentLength="
        + skinDataEntry.content().length()
        + ", timestamp="
        + skinDataEntry.timestamp();
  }
}
