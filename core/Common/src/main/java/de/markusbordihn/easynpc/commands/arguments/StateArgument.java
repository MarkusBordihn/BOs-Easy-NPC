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

package de.markusbordihn.easynpc.commands.arguments;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.SimpleCommandExceptionType;
import de.markusbordihn.easynpc.data.state.StateIdentifier;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.Arrays;
import java.util.Collection;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.resources.ResourceLocation;

public class StateArgument implements ArgumentType<ResourceLocation> {

  public static final SimpleCommandExceptionType ERROR_INVALID_STATE =
      new SimpleCommandExceptionType(TextComponent.getTranslatedTextRaw("argument.id.invalid"));
  private static final Collection<String> EXAMPLES =
      Arrays.asList("quest_progress", "my_mod:stage");

  public StateArgument() {}

  public static StateArgument state() {
    return new StateArgument();
  }

  public static ResourceLocation getStateId(
      final CommandContext<CommandSourceStack> commandContext, final String argumentName) {
    return commandContext.getArgument(argumentName, ResourceLocation.class);
  }

  @Override
  public ResourceLocation parse(final StringReader stringReader) throws CommandSyntaxException {
    int startCursor = stringReader.getCursor();
    while (stringReader.canRead()
        && ResourceLocation.isAllowedInResourceLocation(stringReader.peek())) {
      stringReader.skip();
    }

    ResourceLocation stateId =
        StateIdentifier.parse(
            stringReader.getString().substring(startCursor, stringReader.getCursor()));
    if (stateId == null) {
      stringReader.setCursor(startCursor);
      throw ERROR_INVALID_STATE.createWithContext(stringReader);
    }

    return stateId;
  }

  @Override
  public Collection<String> getExamples() {
    return EXAMPLES;
  }
}
