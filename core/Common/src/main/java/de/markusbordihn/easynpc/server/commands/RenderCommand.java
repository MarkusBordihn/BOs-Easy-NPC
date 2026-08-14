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
import de.markusbordihn.easynpc.api.texture.ModelTextureAPI;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.commands.arguments.EntityTypeArgument;
import de.markusbordihn.easynpc.commands.suggestion.RenderModelSuggestions;
import de.markusbordihn.easynpc.commands.suggestion.RenderTypeSuggestions;
import de.markusbordihn.easynpc.data.configuration.ConfigurationData;
import de.markusbordihn.easynpc.data.render.ModelTextureBlend;
import de.markusbordihn.easynpc.data.render.ModelTextureSetting;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ConfigurationDataCapable;
import de.markusbordihn.easynpc.handler.RenderHandler;
import java.util.List;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;

public class RenderCommand extends Command {

  private static final String TEXTURE_ARG = "texture";
  private static final String BLEND_ARG = "blend";

  private RenderCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("render")
        .requires(commandSource -> commandSource.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.literal("type")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                                .then(
                                    Commands.argument(TYPE_ARG, StringArgumentType.string())
                                        .suggests(RenderTypeSuggestions::suggest)
                                        .executes(
                                            context ->
                                                setRenderType(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    RenderType.get(
                                                        StringArgumentType.getString(
                                                            context, TYPE_ARG)))))))
                .then(
                    Commands.literal("model")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                                .then(
                                    Commands.argument(ENTITY_ARG, EntityTypeArgument.entityType())
                                        .executes(
                                            context ->
                                                setRenderEntityType(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    EntityTypeArgument.getEntityType(
                                                        context, ENTITY_ARG))))))
                .then(
                    Commands.literal("species")
                        .requires(
                            commandSourceStack ->
                                commandSourceStack.hasPermission(Commands.LEVEL_ALL))
                        .then(
                            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                                .then(
                                    Commands.argument(SPECIES_ARG, StringArgumentType.string())
                                        .suggests(RenderModelSuggestions::suggest)
                                        .executes(
                                            context ->
                                                setRenderEntityModel(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    StringArgumentType.getString(
                                                        context, SPECIES_ARG)))))))
        .then(textureArguments());
  }

  private static ArgumentBuilder<CommandSourceStack, ?> textureArguments() {
    return Commands.literal("texture")
        .requires(commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .then(
                            Commands.argument(SLOT_ARG, StringArgumentType.word())
                                .then(
                                    Commands.argument(TEXTURE_ARG, ResourceLocationArgument.id())
                                        .executes(
                                            context ->
                                                setTexture(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    StringArgumentType.getString(context, SLOT_ARG),
                                                    ResourceLocationArgument.getId(
                                                        context, TEXTURE_ARG),
                                                    ModelTextureBlend.DEFAULT))
                                        .then(
                                            Commands.argument(BLEND_ARG, StringArgumentType.word())
                                                .suggests(
                                                    (context, builder) ->
                                                        SharedSuggestionProvider.suggest(
                                                            ModelTextureBlend.serializedNames(),
                                                            builder))
                                                .executes(
                                                    context ->
                                                        setTexture(
                                                            context.getSource(),
                                                            EasyNPCArgument.getEntityWithAccess(
                                                                context, NPC_TARGET_ARG),
                                                            StringArgumentType.getString(
                                                                context, SLOT_ARG),
                                                            ResourceLocationArgument.getId(
                                                                context, TEXTURE_ARG),
                                                            textureBlend(
                                                                context.getSource(),
                                                                StringArgumentType.getString(
                                                                    context, BLEND_ARG)))))))))
        .then(
            Commands.literal("clear")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                clearTextures(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)))
                        .then(
                            Commands.argument(SLOT_ARG, StringArgumentType.word())
                                .executes(
                                    context ->
                                        clearTexture(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            StringArgumentType.getString(context, SLOT_ARG))))))
        .then(
            Commands.literal("query")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                queryTextures(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(
                                        context, NPC_TARGET_ARG)))));
  }

  private static int setRenderType(
      CommandSourceStack context, EasyNPC<?> easyNPC, RenderType renderType) {
    if (easyNPC == null || renderType == null) {
      return 0;
    }

    // Set render type.
    if (!RenderHandler.setRenderType(easyNPC, renderType)) {
      return sendFailureMessage(
          context, "Failed to set render type " + renderType + " for EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(context, "Set render type " + renderType + " for EasyNPC " + easyNPC);
  }

  private static int setRenderEntityType(
      CommandSourceStack context, EasyNPC<?> easyNPC, EntityType<? extends Entity> entityType) {
    if (easyNPC == null || entityType == null) {
      return 0;
    }

    if (!isDopplerNPC(easyNPC)) {
      return sendFailureMessage(
          context,
          "Custom models can only be set on Doppler NPCs. Current NPC type: "
              + easyNPC.getEntity().getType().getDescriptionId());
    }

    if (!RenderHandler.setRenderEntity(easyNPC, entityType)) {
      return sendFailureMessage(
          context, "Failed to set render entity " + entityType + " for EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(
        context,
        "Set render entity " + entityType + " for EasyNPC with UUID " + easyNPC.getEntityUUID());
  }

  private static int setRenderEntityModel(
      CommandSourceStack context, EasyNPC<?> easyNPC, String speciesId) {
    if (easyNPC == null || speciesId == null || speciesId.isEmpty()) {
      return 0;
    }

    if (!isCobblemonNPC(easyNPC) && !isEasyModelNPC(easyNPC)) {
      return sendFailureMessage(
          context,
          "Species can only be set on Cobblemon or Easy Model NPCs. Current NPC type: "
              + easyNPC.getEntity().getType().getDescriptionId());
    }

    if (!RenderHandler.setRenderEntityModel(easyNPC, speciesId)) {
      return sendFailureMessage(
          context, "Failed to set species " + speciesId + " for EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(
        context, "Set species " + speciesId + " for EasyNPC with UUID " + easyNPC.getEntityUUID());
  }

  private static int setTexture(
      CommandSourceStack context,
      EasyNPC<?> easyNPC,
      String slot,
      ResourceLocation texture,
      ModelTextureBlend blend) {
    if (easyNPC == null || texture == null || blend == null) {
      return FAILURE;
    }

    if (!supportsTextures(context, easyNPC)) {
      return FAILURE;
    }

    if (!ModelTextureAPI.setTexture(easyNPC, slot, texture, blend)) {
      return sendFailureMessage(
          context, "Failed to set texture " + texture + " for slot " + slot + " of " + easyNPC);
    }

    return sendSuccessMessage(
        context, "Set texture " + texture + " for slot " + slot + " of EasyNPC " + easyNPC);
  }

  private static int clearTexture(CommandSourceStack context, EasyNPC<?> easyNPC, String slot) {
    if (easyNPC == null || !supportsTextures(context, easyNPC)) {
      return FAILURE;
    }

    if (!ModelTextureAPI.clearTexture(easyNPC, slot)) {
      return sendFailureMessage(
          context, "No texture set for slot " + slot + " of EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(
        context, "Cleared texture for slot " + slot + " of EasyNPC " + easyNPC);
  }

  private static int clearTextures(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null || !supportsTextures(context, easyNPC)) {
      return FAILURE;
    }

    if (!ModelTextureAPI.clearTextures(easyNPC)) {
      return sendFailureMessage(context, "No textures set for EasyNPC " + easyNPC);
    }

    return sendSuccessMessage(context, "Cleared all textures for EasyNPC " + easyNPC);
  }

  private static int queryTextures(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null || !supportsTextures(context, easyNPC)) {
      return FAILURE;
    }

    List<String> availableSlots = ModelTextureAPI.listTextureSlots(easyNPC);
    if (!availableSlots.isEmpty()) {
      sendSuccessMessage(context, "Available texture slots: " + String.join(", ", availableSlots));
    }

    ModelTextureSetting textureSetting = ModelTextureAPI.getTextureSetting(easyNPC);
    if (textureSetting.isEmpty()) {
      return sendSuccessMessage(context, "No texture override set for EasyNPC " + easyNPC);
    }

    textureSetting
        .slots()
        .forEach(
            (slot, textureSlot) ->
                sendSuccessMessage(
                    context,
                    slot
                        + ": "
                        + textureSlot.texture().map(ResourceLocation::toString).orElse("-")
                        + " ("
                        + textureSlot.blend().getSerializedName()
                        + ")"));
    return SINGLE_SUCCESS;
  }

  private static ModelTextureBlend textureBlend(CommandSourceStack context, String value) {
    ModelTextureBlend blend = ModelTextureBlend.parse(value).orElse(null);
    if (blend == null) {
      sendFailureMessage(context, "Unknown texture blend " + value);
    }

    return blend;
  }

  private static boolean supportsTextures(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (ModelTextureAPI.supportsTextures(easyNPC)) {
      return true;
    }

    sendFailureMessage(
        context,
        "Textures can only be set on Easy Model NPCs. Current NPC type: "
            + easyNPC.getEntity().getType().getDescriptionId());
    return false;
  }

  private static boolean isDopplerNPC(EasyNPC<?> easyNPC) {
    return easyNPC instanceof ConfigurationDataCapable<?> configurable
        && configurable.getConfigurationData() == ConfigurationData.DOPPLER;
  }

  private static boolean isCobblemonNPC(EasyNPC<?> easyNPC) {
    return easyNPC instanceof ConfigurationDataCapable<?> configurable
        && configurable.getConfigurationData() == ConfigurationData.COBBLEMON;
  }

  private static boolean isEasyModelNPC(EasyNPC<?> easyNPC) {
    return easyNPC instanceof ConfigurationDataCapable<?> configurable
        && (configurable.getConfigurationData() == ConfigurationData.EASY_MODEL
            || configurable.getConfigurationData() == ConfigurationData.EASY_MODEL_HUMANOID);
  }
}
