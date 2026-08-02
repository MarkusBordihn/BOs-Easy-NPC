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

import com.mojang.brigadier.arguments.BoolArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.commands.arguments.StateArgument;
import de.markusbordihn.easynpc.commands.suggestion.StateSuggestions;
import de.markusbordihn.easynpc.data.state.StateDataSet;
import de.markusbordihn.easynpc.data.state.StateEntry;
import de.markusbordihn.easynpc.data.state.StateOperation;
import de.markusbordihn.easynpc.data.state.StateValueType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.StateDataCapable;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.resources.Identifier;
import net.minecraft.server.permissions.Permissions;

public class StateCommand extends Command {

  private static final String STATE_ARG = "state";
  private static final String AMOUNT_ARG = "amount";

  private StateCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("state")
        .requires(cs -> cs.permissions().hasPermission(Permissions.COMMANDS_GAMEMASTER))
        .then(Commands.literal("get").then(stateArgument(StateCommand::getState)))
        .then(
            Commands.literal("list")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                listStates(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG)))))
        .then(Commands.literal("set").then(setArgument()))
        .then(Commands.literal("increase").then(changeArgument(1)))
        .then(Commands.literal("decrease").then(changeArgument(-1)))
        .then(Commands.literal("toggle").then(stateArgument(StateCommand::toggleState)))
        .then(Commands.literal("remove").then(stateArgument(StateCommand::removeState)));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> stateArgument(StateAction stateAction) {
    return Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
        .then(
            Commands.argument(STATE_ARG, StateArgument.state())
                .suggests(StateSuggestions::suggest)
                .executes(
                    context ->
                        stateAction.apply(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                            StateArgument.getStateId(context, STATE_ARG))));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> setArgument() {
    return Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
        .then(
            Commands.argument(STATE_ARG, StateArgument.state())
                .suggests(StateSuggestions::suggest)
                .then(
                    Commands.argument(VALUE_ARG, IntegerArgumentType.integer())
                        .executes(
                            context ->
                                setState(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    StateArgument.getStateId(context, STATE_ARG),
                                    StateEntry.of(
                                        IntegerArgumentType.getInteger(context, VALUE_ARG)))))
                .then(
                    Commands.literal("text")
                        .then(
                            Commands.argument(VALUE_ARG, StringArgumentType.string())
                                .executes(
                                    context ->
                                        setState(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            StateArgument.getStateId(context, STATE_ARG),
                                            StateEntry.of(
                                                StringArgumentType.getString(
                                                    context, VALUE_ARG))))))
                .then(
                    Commands.literal("flag")
                        .then(
                            Commands.argument(VALUE_ARG, BoolArgumentType.bool())
                                .executes(
                                    context ->
                                        setState(
                                            context.getSource(),
                                            EasyNPCArgument.getEntityWithAccess(
                                                context, NPC_TARGET_ARG),
                                            StateArgument.getStateId(context, STATE_ARG),
                                            StateEntry.of(
                                                BoolArgumentType.getBool(context, VALUE_ARG)))))));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> changeArgument(int signum) {
    return Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
        .then(
            Commands.argument(STATE_ARG, StateArgument.state())
                .suggests(StateSuggestions::suggest)
                .executes(
                    context ->
                        changeState(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                            StateArgument.getStateId(context, STATE_ARG),
                            signum))
                .then(
                    Commands.argument(AMOUNT_ARG, IntegerArgumentType.integer(1))
                        .executes(
                            context ->
                                changeState(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    StateArgument.getStateId(context, STATE_ARG),
                                    signum
                                        * IntegerArgumentType.getInteger(context, AMOUNT_ARG)))));
  }

  private static int getState(CommandSourceStack context, EasyNPC<?> easyNPC, Identifier stateId) {
    StateDataCapable<?> stateData = resolveStateData(context, easyNPC);
    if (stateData == null) {
      return FAILURE;
    }

    StateEntry stateEntry = stateData.getState(stateId);
    if (stateEntry == null) {
      return sendSuccessMessage(context, "State " + stateId + " is not set for " + easyNPC);
    }

    return sendSuccessMessage(
        context, "State " + stateId + " of " + easyNPC + " is " + describe(stateEntry));
  }

  private static int listStates(CommandSourceStack context, EasyNPC<?> easyNPC) {
    StateDataCapable<?> stateData = resolveStateData(context, easyNPC);
    if (stateData == null) {
      return FAILURE;
    }

    StateDataSet stateDataSet = stateData.getStateDataSet();
    if (stateDataSet == null || stateDataSet.isEmpty()) {
      return sendSuccessMessage(context, "No states are set for " + easyNPC);
    }

    StringBuilder message =
        new StringBuilder("States of ")
            .append(easyNPC)
            .append(" (")
            .append(easyNPC.getEntityUUID())
            .append("):");
    for (Identifier stateId : stateDataSet.keys()) {
      StateEntry stateEntry = stateDataSet.get(stateId);
      message
          .append("\n- ")
          .append(stateId)
          .append(" = ")
          .append(describe(stateEntry))
          .append(" (")
          .append(StateValueType.of(stateEntry))
          .append(')');
    }

    return sendSuccessMessage(context, message.toString());
  }

  private static int setState(
      CommandSourceStack context, EasyNPC<?> easyNPC, Identifier stateId, StateEntry stateEntry) {
    StateDataCapable<?> stateData = resolveStateData(context, easyNPC);
    if (stateData == null) {
      return FAILURE;
    }

    stateData.setState(stateId, stateEntry);
    return sendSuccessMessage(
        context, "Set state " + stateId + " of " + easyNPC + " to " + describe(stateEntry));
  }

  private static int changeState(
      CommandSourceStack context, EasyNPC<?> easyNPC, Identifier stateId, int amount) {
    StateDataCapable<?> stateData = resolveStateData(context, easyNPC);
    if (stateData == null) {
      return FAILURE;
    }

    stateData.setState(
        stateId,
        StateOperation.INCREASE.apply(stateData.getState(stateId), String.valueOf(amount)));
    return sendSuccessMessage(
        context,
        "Changed state " + stateId + " of " + easyNPC + " to " + stateData.getStateNumber(stateId));
  }

  private static int toggleState(
      CommandSourceStack context, EasyNPC<?> easyNPC, Identifier stateId) {
    StateDataCapable<?> stateData = resolveStateData(context, easyNPC);
    if (stateData == null) {
      return FAILURE;
    }

    stateData.setState(stateId, !stateData.getStateFlag(stateId));
    return sendSuccessMessage(
        context,
        "Toggled state " + stateId + " of " + easyNPC + " to " + stateData.getStateFlag(stateId));
  }

  private static int removeState(
      CommandSourceStack context, EasyNPC<?> easyNPC, Identifier stateId) {
    StateDataCapable<?> stateData = resolveStateData(context, easyNPC);
    if (stateData == null) {
      return FAILURE;
    }

    stateData.removeState(stateId);
    return sendSuccessMessage(context, "Removed state " + stateId + " from " + easyNPC);
  }

  private static String describe(StateEntry stateEntry) {
    return stateEntry.isText() ? stateEntry.textValue() : String.valueOf(stateEntry.numberValue());
  }

  private static StateDataCapable<?> resolveStateData(
      CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (easyNPC == null) {
      return null;
    }

    StateDataCapable<?> stateData = easyNPC.getEasyNPCStateData();
    if (stateData == null) {
      sendFailureMessage(context, easyNPC + " does not support states");
    }

    return stateData;
  }

  private interface StateAction {
    int apply(CommandSourceStack context, EasyNPC<?> easyNPC, Identifier stateId);
  }
}
