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

import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.handler.SkinHandler;
import de.markusbordihn.easynpc.io.ServerSkinIndex;
import de.markusbordihn.easynpc.io.ServerSkinIndex.ServerSkin;
import de.markusbordihn.easynpc.utils.SkinFileNameUtils;
import java.util.List;
import java.util.UUID;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;

public class SkinCommand extends Command {

  private SkinCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("skin")
        .requires(commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.literal("variant")
                        .requires(commandSourceStack ->
                            commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                                .then(
                                    Commands.argument(VARIANT_ARG, StringArgumentType.word())
                                        .suggests((context, builder) -> {
                                          EasyNPC<?> easyNPC =
                                              EasyNPCArgument.getEntityWithAccess(
                                                  context, NPC_TARGET_ARG);
                                          if (easyNPC instanceof EasyNPCBase<?> easyNPCBase) {
                                            Enum<?>[] variants = easyNPCBase.getSkinVariantTypes();
                                            for (Enum<?> variant : variants) {
                                              builder.suggest(variant.name());
                                            }
                                          }
                                          return builder.buildFuture();
                                        })
                                        .executes(context ->
                                            setDefaultSkinVariant(
                                                context.getSource(),
                                                EasyNPCArgument.getEntityWithAccess(
                                                    context, NPC_TARGET_ARG),
                                                StringArgumentType.getString(
                                                    context, VARIANT_ARG))))))
                .then(
                    Commands.literal("custom")
                        .then(
                            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                                .then(
                                    Commands.argument(NAME_ARG, StringArgumentType.greedyString())
                                        .suggests((context, builder) -> {
                                          EasyNPC<?> easyNPC =
                                              EasyNPCArgument.getEntityWithAccess(
                                                  context, NPC_TARGET_ARG);
                                          if (easyNPC != null
                                              && easyNPC.getEasyNPCSkinData() != null) {
                                            for (ServerSkin serverSkin :
                                                ServerSkinIndex.getSkins(
                                                    easyNPC.getEasyNPCSkinData().getSkinModel())) {
                                              builder.suggest(serverSkin.displayName());
                                            }
                                          }
                                          return builder.buildFuture();
                                        })
                                        .executes(context ->
                                            setCustomSkin(
                                                context.getSource(),
                                                EasyNPCArgument.getEntityWithAccess(
                                                    context, NPC_TARGET_ARG),
                                                StringArgumentType.getString(
                                                    context, NAME_ARG)))))))
        .then(
            Commands.literal("layer")
                .requires(
                    commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(context ->
                            getLayerStatus(
                                context.getSource(),
                                EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)))
                        .then(
                            Commands.literal("disable")
                                .executes(context ->
                                    disableLayers(
                                        context.getSource(),
                                        EasyNPCArgument.getEntityWithAccess(
                                            context, NPC_TARGET_ARG))))
                        .then(
                            Commands.literal("enable")
                                .executes(context ->
                                    enableLayers(
                                        context.getSource(),
                                        EasyNPCArgument.getEntityWithAccess(
                                            context, NPC_TARGET_ARG))))));
  }

  private static int setDefaultSkinVariant(
      CommandSourceStack context, EasyNPC<?> easyNPC, String variant) {
    if (easyNPC == null || variant == null || variant.isEmpty()) {
      return 0;
    }

    if (!SkinHandler.setSkin(easyNPC, SkinDataEntry.createDefaultSkin(variant))) {
      return sendFailureMessage(
          context, "Failed to set skin variant " + variant + " for EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(
        context, "Successfully set skin variant " + variant + " for EasyNPC " + easyNPC);
  }

  private static int setCustomSkin(
      CommandSourceStack context, EasyNPC<?> easyNPC, String skinName) {
    if (easyNPC == null || skinName == null || skinName.isBlank()) {
      return sendFailureMessage(context, "Invalid EasyNPC target or skin name");
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      return sendFailureMessage(context, "Invalid EasyNPC skin data");
    }

    SkinModel skinModel = skinData.getSkinModel();
    List<ServerSkin> serverSkins = ServerSkinIndex.findSkins(skinName);
    UUID skinUUID =
        serverSkins.isEmpty()
            ? SkinFileNameUtils.getSkinUUID(SkinFileNameUtils.withPngExtension(skinName))
            : serverSkins.stream()
                .filter(serverSkin -> serverSkin.skinModel() == skinModel)
                .map(ServerSkin::skinUUID)
                .findFirst()
                .orElse(null);
    if (skinUUID == null) {
      return sendFailureMessage(
          context,
          "Custom skin " + skinName + " is not available for skin model " + skinModel.getName());
    }

    if (!SkinHandler.setSkin(
        easyNPC,
        SkinDataEntry.createCustomSkin(skinUUID, skinData.getSkinDataEntry().disableLayers()))) {
      return sendFailureMessage(context, "Failed to set custom skin " + skinName);
    }

    String entityName = easyNPC.getEntity().getDisplayName().getString();
    return sendSuccessMessage(
        context,
        serverSkins.isEmpty()
            ? "Set custom skin "
                + skinName
                + " for EasyNPC "
                + entityName
                + ", the server has no such skin, so players need the file locally."
            : "Successfully set custom skin " + skinName + " for EasyNPC " + entityName);
  }

  private static int setLayers(CommandSourceStack context, EasyNPC<?> easyNPC, boolean enabled) {
    if (easyNPC == null) {
      return sendFailureMessage(context, "Invalid EasyNPC target");
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      return sendFailureMessage(context, "Invalid EasyNPC skin data");
    }

    if (!SkinHandler.setSkin(easyNPC, skinData.getSkinDataEntry().withDisableLayers(!enabled))) {
      return sendFailureMessage(context, "Failed to update layers for EasyNPC");
    }

    String action = enabled ? "enabled" : "disabled";
    String entityName = easyNPC.getEntity().getDisplayName().getString();

    return sendSuccessMessage(
        context, "Successfully " + action + " layers for EasyNPC " + entityName);
  }

  private static int disableLayers(CommandSourceStack context, EasyNPC<?> easyNPC) {
    return setLayers(context, easyNPC, false);
  }

  private static int enableLayers(CommandSourceStack context, EasyNPC<?> easyNPC) {
    return setLayers(context, easyNPC, true);
  }

  private static int getLayerStatus(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return sendFailureMessage(context, "Invalid EasyNPC target");
    }

    SkinDataCapable<?> skinData = easyNPC.getEasyNPCSkinData();
    if (skinData == null) {
      return sendFailureMessage(context, "EasyNPC has no skin data");
    }

    boolean layersDisabled = skinData.getSkinDataEntry().disableLayers();
    String status = layersDisabled ? "disabled" : "enabled";
    String entityName = easyNPC.getEntity().getDisplayName().getString();

    return sendSuccessMessage(
        context, "Layers are currently " + status + " for EasyNPC " + entityName);
  }
}
