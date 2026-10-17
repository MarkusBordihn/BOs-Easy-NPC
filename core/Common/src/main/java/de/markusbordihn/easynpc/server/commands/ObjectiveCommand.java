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

import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.server.commands.objectives.AttackObjective;
import de.markusbordihn.easynpc.server.commands.objectives.FollowObjective;
import de.markusbordihn.easynpc.server.commands.objectives.ShoulderObjective;
import java.util.concurrent.CompletableFuture;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;

public class ObjectiveCommand extends Command {

  private static final String OBJECTIVE_ARG = "objective";
  private static final String PRIORITY_ARG = "priority";

  private ObjectiveCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("objective")
        .then(
            Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                .then(
                    Commands.literal("list")
                        .then(AttackObjective.registerList())
                        .then(FollowObjective.registerList()))
                .then(
                    Commands.literal("set")
                        .then(AttackObjective.registerSet())
                        .then(FollowObjective.registerSet())
                        .then(ShoulderObjective.registerSet()))
                .then(
                    Commands.literal("remove")
                        .then(AttackObjective.registerRemove())
                        .then(FollowObjective.registerRemove()))
                .then(
                    Commands.literal("priority")
                        .then(
                            Commands.argument(OBJECTIVE_ARG, StringArgumentType.string())
                                .suggests(ObjectiveCommand::suggestObjectiveIds)
                                .then(
                                    Commands.argument(
                                            PRIORITY_ARG, IntegerArgumentType.integer(0))
                                        .executes(
                                            context ->
                                                setPriority(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, NPC_TARGET_ARG),
                                                    StringArgumentType.getString(
                                                        context, OBJECTIVE_ARG),
                                                    IntegerArgumentType.getInteger(
                                                        context, PRIORITY_ARG)))))));
  }

  public static int setPriority(
      CommandSourceStack context, EasyNPC<?> easyNPC, String objective, int priority) {
    String objectiveId = resolveObjectiveId(easyNPC, objective);
    if (!EasyNPCObjectiveHandler.setPriority(easyNPC, objectiveId, priority)) {
      return sendFailureMessage(context, "Objective " + objective + " not found for " + easyNPC);
    }

    return sendSuccessMessage(
        context, "Set priority of objective " + objectiveId + " to " + priority + " for " + easyNPC);
  }

  private static String resolveObjectiveId(EasyNPC<?> easyNPC, String objective) {
    if (EasyNPCObjectiveHandler.getObjective(easyNPC, objective) != null) {
      return objective;
    }

    return ObjectiveType.get(objective).name();
  }

  private static CompletableFuture<Suggestions> suggestObjectiveIds(
      CommandContext<CommandSourceStack> context, SuggestionsBuilder builder)
      throws CommandSyntaxException {
    EasyNPC<?> easyNPC = EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG);
    return SharedSuggestionProvider.suggest(
        EasyNPCObjectiveHandler.getObjectives(easyNPC).stream()
            .map(ObjectiveDataEntry::getId)
            .map(StringArgumentType::escapeIfRequired),
        builder);
  }
}
