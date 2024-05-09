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
import de.markusbordihn.easynpc.commands.suggestion.SoundTypeSuggestions;
import de.markusbordihn.easynpc.data.sound.SoundDataSet;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundData;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.Commands;
import net.minecraft.commands.arguments.ResourceLocationArgument;
import net.minecraft.commands.synchronization.SuggestionProviders;
import net.minecraft.resources.ResourceLocation;

public class SoundCommand extends Command {

  private SoundCommand() {}

  public static ArgumentBuilder<CommandSourceStack, ?> register() {
    return Commands.literal("sound")
        .requires(cs -> cs.hasPermission(Commands.LEVEL_ALL))
        .then(
            Commands.literal("set")
                .then(
                    Commands.argument("target", new EasyNPCArgument())
                        .then(
                            Commands.argument("type", StringArgumentType.string())
                                .suggests(SoundTypeSuggestions::suggest)
                                .then(
                                    Commands.argument("sound", ResourceLocationArgument.id())
                                        .suggests(SuggestionProviders.AVAILABLE_SOUNDS)
                                        .executes(
                                            context ->
                                                setSoundType(
                                                    context.getSource(),
                                                    EasyNPCArgument.getEntityWithAccess(
                                                        context, "target"),
                                                    SoundType.get(
                                                        StringArgumentType.getString(
                                                            context, "type")),
                                                    ResourceLocationArgument.getId(
                                                        context, "sound")))))));
  }

  private static int setSoundType(
      CommandSourceStack context, EasyNPC<?> easyNPC, SoundType soundType, ResourceLocation sound) {
    if (easyNPC == null || soundType == null || sound == null) {
      return 0;
    }

    // Verify sound data
    SoundData<?> soundData = easyNPC.getEasyNPCSoundData();
    if (soundData == null) {
      return sendFailureMessageNoSoundData(context, easyNPC);
    }

    // Verify sound data settings
    SoundDataSet soundDataSet = soundData.getSoundDataSet();
    if (soundDataSet == null) {
      return sendFailureMessageNoSoundDataSet(context, easyNPC);
    }

    // Set sound type and refresh data set.
    soundDataSet.addSound(soundType, sound);
    soundData.refreshSoundDataSet();

    return sendSuccessMessage(
        context,
        "Sound type "
            + soundType
            + " for EasyNPC "
            + easyNPC.getUUID()
            + " was set to "
            + sound
            + " !");
  }
}
