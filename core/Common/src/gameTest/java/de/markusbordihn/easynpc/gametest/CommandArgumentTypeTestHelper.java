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

package de.markusbordihn.easynpc.gametest;

import com.mojang.brigadier.tree.ArgumentCommandNode;
import com.mojang.brigadier.tree.CommandNode;
import de.markusbordihn.easynpc.Constants;
import java.util.ArrayList;
import java.util.List;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.synchronization.ArgumentTypeInfos;
import net.minecraft.gametest.framework.GameTestHelper;

/**
 * Guards the command tree that is sent to a joining player.
 *
 * <p>An argument type that is not registered for synchronization lets every player join fail with
 * "Couldn't place player in world", which no other test notices because a mock player never
 * receives the command tree.
 */
public class CommandArgumentTypeTestHelper {

  private CommandArgumentTypeTestHelper() {}

  public static void assertEveryCommandArgumentCanBeSynchronized(GameTestHelper helper) {
    CommandNode<CommandSourceStack> modCommand =
        helper
            .getLevel()
            .getServer()
            .getCommands()
            .getDispatcher()
            .getRoot()
            .getChild(Constants.MOD_COMMAND);
    GameTestHelpers.assertNotNull(
        helper, "The " + Constants.MOD_COMMAND + " command must be registered", modCommand);

    List<String> unknownArgumentTypes = new ArrayList<>();
    collectUnknownArgumentTypes(modCommand, unknownArgumentTypes);

    GameTestHelpers.assertTrue(
        helper,
        "Every command argument type must be registered for synchronization, missing: "
            + unknownArgumentTypes,
        unknownArgumentTypes.isEmpty());
  }

  private static void collectUnknownArgumentTypes(
      CommandNode<CommandSourceStack> commandNode, List<String> unknownArgumentTypes) {
    if (commandNode instanceof ArgumentCommandNode<CommandSourceStack, ?> argumentCommandNode) {
      Class<?> argumentTypeClass = argumentCommandNode.getType().getClass();
      if (!ArgumentTypeInfos.isClassRecognized(argumentTypeClass)
          && !unknownArgumentTypes.contains(argumentTypeClass.getName())) {
        unknownArgumentTypes.add(argumentTypeClass.getName());
      }
    }

    for (CommandNode<CommandSourceStack> childNode : commandNode.getChildren()) {
      collectUnknownArgumentTypes(childNode, unknownArgumentTypes);
    }
  }
}
