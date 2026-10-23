/*
 * Copyright 2026 Markus Bordihn
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
import de.markusbordihn.easynpc.commands.suggestion.CustomIdentifierNamespaceSuggestions;
import de.markusbordihn.easynpc.commands.suggestion.CustomIdentifierSuggestions;
import de.markusbordihn.easynpc.commands.suggestion.DimensionSuggestions;
import de.markusbordihn.easynpc.commands.suggestion.NPCEntityTypeSuggestions;
import de.markusbordihn.easynpc.data.npc.NPCEntityMetadata;
import de.markusbordihn.easynpc.data.saveddata.NPCEntityData;
import java.util.Map;
import java.util.UUID;
import net.minecraft.ChatFormatting;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.EntityArgument;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerPlayer;

public class ListCommand extends Command {

  private static final String OWNER_ARG = "owner";
  private static final String ENTITY_TYPE_ARG = "entity_type";
  private static final String DIMENSION_ARG = "dimension";
  private static final String CUSTOM_IDENTIFIER_ARG = "custom_identifier";
  private static final String NAMESPACE_ARG = "namespace";

  private ListCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("list")
        .requires(
            commandSourceStack -> commandSourceStack.hasPermission(Commands.LEVEL_GAMEMASTERS))
        .executes(context -> listAllNPCs(context.getSource()))
        .then(
            Commands.literal("owner")
                .executes(
                    context ->
                        listNPCsByOwner(
                            context.getSource(), context.getSource().getPlayerOrException()))
                .then(
                    Commands.argument(OWNER_ARG, EntityArgument.player())
                        .executes(
                            context ->
                                listNPCsByOwner(
                                    context.getSource(),
                                    EntityArgument.getPlayer(context, OWNER_ARG)))))
        .then(
            Commands.literal("type")
                .then(
                    Commands.argument(ENTITY_TYPE_ARG, ResourceLocationArgument.id())
                        .suggests(NPCEntityTypeSuggestions::suggest)
                        .executes(
                            context ->
                                listNPCsByType(
                                    context.getSource(),
                                    ResourceLocationArgument.getId(context, ENTITY_TYPE_ARG)))))
        .then(
            Commands.literal("dimension")
                .then(
                    Commands.argument(DIMENSION_ARG, ResourceLocationArgument.id())
                        .suggests(DimensionSuggestions::suggest)
                        .executes(
                            context ->
                                listNPCsByDimension(
                                    context.getSource(),
                                    ResourceLocationArgument.getId(context, DIMENSION_ARG)))))
        .then(
            Commands.literal("identifier")
                .then(
                    Commands.argument(CUSTOM_IDENTIFIER_ARG, ResourceLocationArgument.id())
                        .suggests(CustomIdentifierSuggestions::suggest)
                        .executes(
                            context ->
                                listNPCsByCustomIdentifier(
                                    context.getSource(),
                                    ResourceLocationArgument.getId(
                                        context, CUSTOM_IDENTIFIER_ARG)))))
        .then(
            Commands.literal("namespace")
                .then(
                    Commands.argument(NAMESPACE_ARG, StringArgumentType.word())
                        .suggests(CustomIdentifierNamespaceSuggestions::suggest)
                        .executes(
                            context ->
                                listNPCsByNamespace(
                                    context.getSource(),
                                    StringArgumentType.getString(context, NAMESPACE_ARG)))));
  }

  private static int listAllNPCs(CommandSourceStack context) {
    Map<UUID, NPCEntityMetadata> metadataByUUID = NPCEntityData.get().getAllMetadata();
    int count = metadataByUUID.size();
    sendSuccessMessage(context, "Total NPCs: " + count, ChatFormatting.GREEN);

    int displayed = 0;
    for (Map.Entry<UUID, NPCEntityMetadata> metadataEntry : metadataByUUID.entrySet()) {
      if (displayed >= 10) {
        sendSuccessMessage(
            context, "... and " + (count - displayed) + " more NPCs", ChatFormatting.GRAY);
        break;
      }
      sendEntryInfo(context, metadataEntry.getKey(), metadataEntry.getValue(), true, true, false);
      displayed++;
    }
    return Command.SINGLE_SUCCESS;
  }

  private static int listNPCsByOwner(CommandSourceStack context, ServerPlayer player) {
    Map<UUID, NPCEntityMetadata> metadataByUUID =
        NPCEntityData.get().getMetadataByOwner(player.getUUID());

    boolean isSelf =
        context.getEntity() instanceof ServerPlayer self && self.getUUID().equals(player.getUUID());
    String ownerName = isSelf ? "Your" : player.getName().getString() + "'s";

    sendSuccessMessage(
        context, ownerName + " NPCs: " + metadataByUUID.size(), ChatFormatting.GREEN);
    metadataByUUID.forEach(
        (entityUUID, entityMetadata) ->
            sendEntryInfo(context, entityUUID, entityMetadata, false, true, true));
    return Command.SINGLE_SUCCESS;
  }

  private static int listNPCsByType(CommandSourceStack context, ResourceLocation entityType) {
    Map<UUID, NPCEntityMetadata> metadataByUUID =
        NPCEntityData.get().getMetadataByType(entityType.toString());

    sendSuccessMessage(
        context,
        "NPCs of type " + entityType + ": " + metadataByUUID.size(),
        ChatFormatting.GREEN);
    metadataByUUID.forEach(
        (entityUUID, entityMetadata) ->
            sendEntryInfo(context, entityUUID, entityMetadata, true, false, true));
    return Command.SINGLE_SUCCESS;
  }

  private static int listNPCsByDimension(CommandSourceStack context, ResourceLocation dimension) {
    Map<UUID, NPCEntityMetadata> metadataByUUID =
        NPCEntityData.get().getMetadataByDimension(dimension.toString());

    sendSuccessMessage(
        context,
        "NPCs in dimension " + dimension + ": " + metadataByUUID.size(),
        ChatFormatting.GREEN);
    metadataByUUID.forEach(
        (entityUUID, entityMetadata) ->
            sendEntryInfo(context, entityUUID, entityMetadata, true, true, false));
    return Command.SINGLE_SUCCESS;
  }

  private static int listNPCsByCustomIdentifier(
      CommandSourceStack context, ResourceLocation customIdentifier) {
    Map<UUID, NPCEntityMetadata> metadataByUUID =
        NPCEntityData.get().getMetadataByCustomIdentifier(customIdentifier);

    sendSuccessMessage(
        context,
        "NPCs with custom identifier " + customIdentifier + ": " + metadataByUUID.size(),
        ChatFormatting.GREEN);
    metadataByUUID.forEach(
        (entityUUID, entityMetadata) ->
            sendEntryInfo(context, entityUUID, entityMetadata, true, true, true));
    return Command.SINGLE_SUCCESS;
  }

  private static int listNPCsByNamespace(CommandSourceStack context, String namespace) {
    Map<UUID, NPCEntityMetadata> metadataByUUID =
        NPCEntityData.get().getMetadataByCustomIdentifierNamespace(namespace);

    sendSuccessMessage(
        context,
        "NPCs with custom identifier namespace '" + namespace + "': " + metadataByUUID.size(),
        ChatFormatting.GREEN);

    metadataByUUID.forEach(
        (entityUUID, entityMetadata) -> {
          StringBuilder info = new StringBuilder("- " + entityUUID);
          if (entityMetadata.hasOwner()) {
            info.append(" Owner: ").append(entityMetadata.ownerUUID());
          } else {
            info.append(" (No owner)");
          }
          if (entityMetadata.hasEntityType()) {
            info.append(" Type: ").append(entityMetadata.entityType());
          }
          if (entityMetadata.hasCustomIdentifier()) {
            info.append(" ID: ").append(entityMetadata.customIdentifier());
          }
          sendSuccessMessage(context, info.toString(), ChatFormatting.WHITE);
        });
    return Command.SINGLE_SUCCESS;
  }

  private static void sendEntryInfo(
      CommandSourceStack context,
      UUID entityUUID,
      NPCEntityMetadata entityMetadata,
      boolean showOwner,
      boolean showType,
      boolean showDimension) {
    StringBuilder info = new StringBuilder("- " + entityUUID);

    if (showOwner) {
      if (entityMetadata.hasOwner()) {
        info.append(" Owner: ").append(entityMetadata.ownerUUID());
      } else {
        info.append(" (No owner)");
      }
    }
    if (showType && entityMetadata.hasEntityType()) {
      info.append(" Type: ").append(entityMetadata.entityType());
    }
    if (showDimension && entityMetadata.hasDimension()) {
      info.append(" Dimension: ").append(entityMetadata.dimension());
    }

    sendSuccessMessage(context, info.toString(), ChatFormatting.WHITE);
  }
}
