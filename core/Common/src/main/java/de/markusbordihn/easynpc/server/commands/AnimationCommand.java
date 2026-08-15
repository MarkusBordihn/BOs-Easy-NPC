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

import com.mojang.brigadier.arguments.FloatArgumentType;
import com.mojang.brigadier.arguments.IntegerArgumentType;
import com.mojang.brigadier.arguments.StringArgumentType;
import com.mojang.brigadier.builder.ArgumentBuilder;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import de.markusbordihn.easynpc.api.animation.ModelAnimationAPI;
import de.markusbordihn.easynpc.commands.Command;
import de.markusbordihn.easynpc.commands.arguments.EasyNPCArgument;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlayback;
import de.markusbordihn.easynpc.data.model.ModelAnimationPlaybackMode;
import de.markusbordihn.easynpc.data.model.ModelAnimationSwitchTiming;
import de.markusbordihn.easynpc.data.model.ModelAnimationTransition;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.Comparator;
import java.util.Locale;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.SharedSuggestionProvider;

public final class AnimationCommand extends Command {

  private static final String ANIMATION_ARG = "animation";
  private static final String MODE_ARG = "mode";
  private static final String TIMING_ARG = "timing";
  private static final String BLEND_TICKS_ARG = "blend_ticks";
  private static final String REPEAT_COUNT_ARG = "repeat_count";
  private static final String DURATION_TICKS_ARG = "duration_ticks";
  private static final String[] MODE_SUGGESTIONS = {"once", "loop", "repeat"};
  private static final String[] TIMING_SUGGESTIONS = {"immediate", "after_current"};

  private AnimationCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("animation")
        .requires(cs -> true)
        .then(Commands.literal("play").then(playArguments()))
        .then(Commands.literal("stop").then(stopArguments()))
        .then(
            Commands.literal("restart")
                .then(
                    Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
                        .executes(
                            context ->
                                restart(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(
                                        context, NPC_TARGET_ARG)))));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> playArguments() {
    return Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
        .then(
            Commands.argument(ANIMATION_ARG, StringArgumentType.string())
                .suggests(
                    (context, builder) ->
                        SharedSuggestionProvider.suggest(
                            ModelAnimationAPI.standardAnimations().stream()
                                .sorted(Comparator.naturalOrder()),
                            builder))
                .executes(AnimationCommand::play)
                .then(
                    Commands.argument(MODE_ARG, StringArgumentType.word())
                        .suggests(
                            (context, builder) ->
                                SharedSuggestionProvider.suggest(MODE_SUGGESTIONS, builder))
                        .executes(AnimationCommand::play)
                        .then(
                            Commands.argument(TIMING_ARG, StringArgumentType.word())
                                .suggests(
                                    (context, builder) ->
                                        SharedSuggestionProvider.suggest(
                                            TIMING_SUGGESTIONS, builder))
                                .executes(AnimationCommand::play)
                                .then(
                                    Commands.argument(
                                            BLEND_TICKS_ARG, FloatArgumentType.floatArg(0.0F))
                                        .executes(AnimationCommand::play)
                                        .then(
                                            Commands.argument(
                                                    REPEAT_COUNT_ARG,
                                                    IntegerArgumentType.integer(1))
                                                .executes(AnimationCommand::play)
                                                .then(
                                                    Commands.argument(
                                                            DURATION_TICKS_ARG,
                                                            FloatArgumentType.floatArg(0.0F))
                                                        .executes(AnimationCommand::play)))))));
  }

  private static ArgumentBuilder<CommandSourceStack, ?> stopArguments() {
    return Commands.argument(NPC_TARGET_ARG, EasyNPCArgument.npc())
        .executes(
            context ->
                stop(
                    context.getSource(),
                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                    ModelAnimationTransition.DEFAULT))
        .then(
            Commands.argument(TIMING_ARG, StringArgumentType.word())
                .suggests(
                    (context, builder) ->
                        SharedSuggestionProvider.suggest(TIMING_SUGGESTIONS, builder))
                .executes(
                    context ->
                        stop(
                            context.getSource(),
                            EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                            transition(
                                context.getSource(),
                                StringArgumentType.getString(context, TIMING_ARG),
                                ModelAnimationTransition.DEFAULT_BLEND_DURATION_TICKS)))
                .then(
                    Commands.argument(BLEND_TICKS_ARG, FloatArgumentType.floatArg(0.0F))
                        .executes(
                            context ->
                                stop(
                                    context.getSource(),
                                    EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG),
                                    transition(
                                        context.getSource(),
                                        StringArgumentType.getString(context, TIMING_ARG),
                                        FloatArgumentType.getFloat(context, BLEND_TICKS_ARG))))));
  }

  private static ModelAnimationPlaybackMode playbackMode(CommandSourceStack context, String value) {
    try {
      return ModelAnimationPlaybackMode.valueOf(value.toUpperCase(Locale.ROOT));
    } catch (IllegalArgumentException exception) {
      sendFailureMessage(context, "Unknown animation mode " + value);
      return null;
    }
  }

  private static ModelAnimationTransition transition(
      CommandSourceStack context, String value, float blendTicks) {
    try {
      return new ModelAnimationTransition(
          ModelAnimationSwitchTiming.valueOf(value.toUpperCase(Locale.ROOT)), blendTicks);
    } catch (IllegalArgumentException exception) {
      sendFailureMessage(context, "Unknown animation timing " + value);
      return null;
    }
  }

  private static int play(CommandContext<CommandSourceStack> context)
      throws CommandSyntaxException {
    CommandSourceStack source = context.getSource();
    ModelAnimationPlaybackMode playbackMode =
        playbackMode(source, optionalArgument(context, MODE_ARG, String.class, "once"));
    ModelAnimationTransition transition =
        transition(
            source,
            optionalArgument(context, TIMING_ARG, String.class, "immediate"),
            optionalArgument(
                context,
                BLEND_TICKS_ARG,
                Float.class,
                ModelAnimationTransition.DEFAULT_BLEND_DURATION_TICKS));
    if (playbackMode == null || transition == null) {
      return FAILURE;
    }

    ModelAnimationPlayback playback =
        new ModelAnimationPlayback(
            playbackMode,
            optionalArgument(
                context,
                REPEAT_COUNT_ARG,
                Integer.class,
                ModelAnimationPlayback.DEFAULT_REPEAT_COUNT),
            optionalArgument(
                context,
                DURATION_TICKS_ARG,
                Float.class,
                ModelAnimationPlayback.UNLIMITED_DURATION_TICKS));
    EasyNPC<?> easyNPC = EasyNPCArgument.getEntityWithAccess(context, NPC_TARGET_ARG);
    String animationName = StringArgumentType.getString(context, ANIMATION_ARG);
    if (!ModelAnimationAPI.playAnimation(easyNPC, animationName, playback, transition)) {
      return sendFailureMessage(source, "Unable to play animation for " + easyNPC);
    }

    return sendSuccessMessage(source, "Playing animation " + animationName + " for " + easyNPC);
  }

  private static <T> T optionalArgument(
      CommandContext<CommandSourceStack> context, String name, Class<T> type, T fallbackValue) {
    try {
      return context.getArgument(name, type);
    } catch (IllegalArgumentException exception) {
      return fallbackValue;
    }
  }

  private static int stop(
      CommandSourceStack context, EasyNPC<?> easyNPC, ModelAnimationTransition transition) {
    if (transition == null) {
      return FAILURE;
    }

    if (!ModelAnimationAPI.stopAnimation(easyNPC, transition)) {
      return sendFailureMessage(context, "Unable to stop animation for " + easyNPC);
    }
    return sendSuccessMessage(context, "Stopping animation for " + easyNPC);
  }

  private static int restart(CommandSourceStack context, EasyNPC<?> easyNPC) {
    if (!ModelAnimationAPI.restartAnimation(easyNPC)) {
      return sendFailureMessage(context, "Unable to restart animation for " + easyNPC);
    }

    return sendSuccessMessage(context, "Restarting animation for " + easyNPC);
  }
}
